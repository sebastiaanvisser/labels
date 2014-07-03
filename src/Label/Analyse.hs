module Label.Analyse
( Field (..)
, Place
, Subst
, analyseDatatype
)
where

import Control.Applicative
import Control.Arrow
import Data.List (groupBy, sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord
import Language.Haskell.TH

-- N'th field within some constructor.

type Place = (Int, Con)

-- Type substitutions from equality constraints.

type Subst = [(Type, Type)]

-- Field description data type.

data Field = Field
  { _name       :: Maybe Name  -- Name of the field, when there is one.
  , _mono       :: Bool        -- Forced to be mono because of type shared with other fields.
  , _type       :: Type        -- Type of the field.
  , _places     :: [Place]     -- Where does the field occur.
  , _subst      :: Subst       -- Type subsitutions from equality constraints.
  , _unifiableL :: [Con]       -- Constructors the constructors for this field unify with.
  , _unifiableR :: [Con]       -- Constructors that unify with the constructors for this field.
  , _others     :: [Con]       -- All the other constructors.
  , _datatype   :: [Con]       -- All the constructors in the datatype.
  , _totalG     :: Bool        -- Can we conclude the getter can be total?
  , _totalM     :: Bool        -- Can we conclude the modifier can be total?
  } deriving (Eq, Show)

-------------------------------------------------------------------------------

-- Group all the individual fields from all the constructors in a datatype by
-- name. We will try to create a label for every available field, for which we
-- need information about all the fields with the same name.

analyseDatatype :: [Con] -> [Field]
analyseDatatype datatype
  = map (buildField datatype)
  . groupBy byName
  . sortBy (comparing _name)
  . concatMap (descriptions fields)
  $ datatype
  where byName f g = False `fromMaybe` ((==) <$> _name f <*> _name g)
        fields = datatype >>= fieldNamesAndTypes

-- Build a single field description from a list of constructor fields with the
-- same name.

buildField :: [Con] -> [Field] -> Field
buildField datatype fs =
  let places              = _places =<< fs
      ourCons             = snd <$> places
      subst               = _subst =<< fs
      otherCons           = filter (not . (`elem` ourCons)) datatype
      (uniL, uniR, noUni) = partitionUnifiable (head ourCons) otherCons
      field = (head fs)
        { _places     = places
        , _subst      = subst
        , _unifiableL = uniL
        , _unifiableR = uniR
        , _others     = noUni
        , _datatype   = datatype
        , _totalG     = length places == length datatype - length noUni
        , _totalM     = length uniR == 0
        }
   in field

-- Build the initial descriptions for every field in a constructor.

descriptions :: [(Maybe Name, Type)] -> Con -> [Field]
descriptions named con =

  case con of
    NormalC _ fs  -> (\(i, (_, ty))    -> make Nothing  ty i) <$> zip [0..] fs
    RecC _ fs     -> (\(i, (n, _, ty)) -> make (Just n) ty i) <$> zip [0..] fs
    InfixC a _ b  -> (\(i, (_, ty))    -> make Nothing  ty i) <$> [(0, a), (1, b)]
    ForallC x y v -> restore <$> descriptions named v
      where eqs = [ (a, b) | EqualP a b <- y ]
            restore field = field { _places = second (ForallC x y) <$> _places field
                                  , _subst  = eqs ++ _subst field
                                  }

  where -- Make a field description.
        make n ty i = Field n (monomorphic named (n, ty)) ty
                            [(i, con)] [] [] [] [] []
                            False False

-- A field is forced to be monomorphic when there is another field in the
-- constructor that doesn't share the same name, but does mention one of the
-- type variables from this field.

monomorphic :: [(Maybe Name, Type)] -> (Maybe Name, Type) -> Bool
monomorphic named self = any (\x -> any (elem x) others1) selfVars
  where others   = filter (/= self) named
        others1  = typeVariables . snd <$> others
        selfVars = typeVariables (snd self)

-------------------------------------------------------------------------------
-- Simple type unification used to see if the types of contructors are
-- unifiable. This is mostly useful when generating labels for for GADTs.

partitionUnifiable
  :: Con        -- The contructor to unify for.
  -> [Con]      -- The other contructors to unify with.
  -> ( [Con]    -- Do unify left biased.
     , [Con]    -- Do unify right biased.
     , [Con]    -- Don't unify at all.
     )
partitionUnifiable p others =
  ( filter uniL  others
  , filter uniR  others
  , filter noUni others
  )
  where uniL  o = unify o p
        uniR  o = unify p o && not (unify o p)
        noUni o = not (unify p o) && not (unify o p)
        unify = unifiableConstructor

-- Are the type indices of two constructors unifiable?

unifiableConstructor :: Con -> Con -> Bool
unifiableConstructor a b = and (zipWith unifiable (indices a) (indices b))
  where indices con =
          case con of
            NormalC {}    -> []
            RecC    {}    -> []
            InfixC  {}    -> []
            ForallC _ x _ -> [ c | EqualP _ c <- x ]

-- Are two types unifiable? (a very naive definition)

unifiable :: Type -> Type -> Bool
unifiable x y =
  case (x, y) of
    ( VarT _        ,      _        ) -> True
    ( _             , VarT _        ) -> False
    ( AppT a b      , AppT c d      ) -> unifiable a c && unifiable b d
    ( SigT t k      , SigT s j      ) -> unifiable t s && k == j
    ( ForallT _ _ t , ForallT _ _ s ) -> unifiable t s
    ( a             , b             ) -> a == b

-------------------------------------------------------------------------------
-- Helper functions.

typeVariables :: Type -> [Name]
typeVariables = map name . go
  where name (PlainTV  n  ) = n
        name (KindedTV n _) = n
        go ty =
          case ty of
            ForallT ts _ _ -> ts
            AppT a b       -> go a ++ go b
            SigT t _       -> go t
            VarT n         -> [PlainTV n]
            _              -> []

fieldNamesAndTypes :: Con -> [(Maybe Name, Type)]
fieldNamesAndTypes con =
  case con of
    NormalC _ fs  -> (,) Nothing . snd <$> fs
    RecC _ fs     -> (\(n, _, t) -> (Just n, t)) <$> fs
    InfixC a _ b  -> [(Nothing, snd a), (Nothing, snd b)]
    ForallC _ _ v -> fieldNamesAndTypes v

