{-# LANGUAGE TemplateHaskell, TypeOperators #-}

module Label.Codegen
( Label(..)
, labelDeclaration
, generateLabels
)
where

import Control.Applicative
import Control.Monad
import Data.List (delete, nub)
import Data.Maybe (fromMaybe)
import Language.Haskell.TH

import Label.Analyse

import qualified Label.Mono   as Mono
import qualified Label.Core   as Core

-- Intermediate data types.

data Label
 = LabelDecl
     Name        -- The label name.
     DecQ        -- An INLINE pragma for the label.
     [TyVarBndr] -- The type variables requiring forall.
     CxtQ        -- The context.
     TypeQ       -- The type.
     ExpQ        -- The label body.
 | LabelExpr
     [TyVarBndr] -- The type variables requiring forall.
     CxtQ        -- The context.
     TypeQ       -- The type.
     ExpQ        -- The label body.

data Typing = Typing
  Bool         -- Monomorphic type or polymorphic.
  TypeQ        -- The label input type.
  TypeQ        -- The label output type.
  [TyVarBndr]  -- All used type variables.

-------------------------------------------------------------------------------

labelDeclaration :: (String -> String) -> Bool -> Bool -> Dec -> Q [Dec]
labelDeclaration mk sigs inl dec =
  do lbls  <- generateLabels mk dec
     decls <- forM lbls $ \l ->
       case l of
         LabelExpr {} -> return []
         LabelDecl n i v c t b ->
           do bdy <- pure <$> funD n [clause [] (normalB b) []]
              prg <- if inl then pure <$> i else return []
              typ <- if sigs
                       then pure <$> sigD n (forallT v c t)
                       else return []
              return (concat [prg, typ, bdy])
     return (concat decls)

-- Generate the labels for all the record fields in the data type.

generateLabels :: (String -> String) -> Dec -> Q [Label]
generateLabels mk dec =

 do -- Only process data and newtype declarations, filter out all
    -- constructors and the type variables.
    let (name, cons, vars) =
          case dec of
            DataD    _ n vs cs _ -> (n, cs,  vs)
            NewtypeD _ n vs c  _ -> (n, [c], vs)
            _ -> fclError "Can only derive labels for datatypes and newtypes."

        -- We are only interested in labels of record constructors.
        fields = rename <$> analyseDatatype cons

    forM fields (generateLabel name vars)

  where rename field = field { _name = mkName . mk . nameBase <$> _name field }

generateLabel
  :: Name
  -> [TyVarBndr]
  -> Field
  -> Q Label

generateLabel datatype dtVars field =

  do Typing mono tyI tyO _
        <- computeTypes (_mono field) (_type field) datatype dtVars (_subst field)

     let m0 = varT (mkName "m")
         m1 = varT (mkName "n")

         context = cxt (mk _totalG m0 ++ mk _totalM m1)
           where mk t m =
                   case t field of
                      True  -> [classP ''Applicative [m]]
                      False -> [classP ''Alternative [m]]

         ty = if mono
              then [t| Mono.Mono (Core.Label $m0 $m1) $tyI $tyO |]
              else [t| Core.Label $m0 $m1 $tyI $tyO |]

         -- Label expression body.
         body = [| Core.make $g $m |]
           where g = getter   field
                 m = modifier field

     tvs <- nub . binderFromType <$> ty
     return $
       case _name field of
         Nothing -> LabelExpr tvs context ty body
         Just n  ->

           let inline = InlineP n Inline FunLike (FromPhase 0)
            in LabelDecl n (return (PragmaD inline)) tvs context ty body

-------------------------------------------------------------------------------
-- Build the function body for the getter.

getter :: Field -> Q Exp
getter field =

  do let pt = mkName "f"
         wild = if _totalG field then [] else [match wildP (normalB [| empty |]) []]
         mkCase (i, c) = match pat (normalB (appE [| pure |] var)) []
           where (pat, var) = case1 i c
     lamE [varP pt]
          $ caseE (varE pt)
                  (map mkCase (_places field) ++ wild)
  where
  case1 i con =
    case con of
      NormalC c fs  -> let s = take (length fs) in (conP c (s pats), var)
      RecC    c fs  -> let s = take (length fs) in (conP c (s pats), var)
      InfixC  _ c _ -> (infixP (pats !! 0) c (pats !! 1), var)
      ForallC _ _ c -> case1 i c
    where fresh = mkName <$> delete "f" freshNames
          pats1 = varP <$> fresh
          pats  = replicate i wildP ++ [pats1 !! i] ++ repeat wildP
          var   = varE (fresh !! i)

-------------------------------------------------------------------------------
-- Build the function body for the modifier.

modifier :: Field -> Q Exp
modifier field =

  do let pt = mkName "f"
         md = mkName "m"
         mkUpdate (i, c) = match pat (normalB var) []
           where (pat, var) = update i c
         mkKeep c  = match pat (normalB (appE [| pure |] var)) []
           where (pat, var) = keep c
         mkFail c  = match pat (normalB [| empty |]) []
           where pat = fail_ c
     lamE [varP md, varP pt]
          $ caseE (varE pt)
                  ( map mkUpdate (_places field)
                 ++ map mkKeep   (_unifiableL field)
                 ++ map mkFail   (_unifiableR field)
                  )
  where

  update i con =
    case con of
      NormalC c fs  -> let s = take (length fs) in (conP c (s pats), apps (conE c) (s vars))
      RecC    c fs  -> let s = take (length fs) in (conP c (s pats), apps (conE c) (s vars))
      InfixC  _ c _ -> ( infixP (pats !! 0) c (pats !! 1)
                       , apps (conE c) (take 2 vars)
                       )
      ForallC _ _ c -> update i c
    where fresh     = mkName <$> delete "f" (delete "m" freshNames)
          pats      = varP <$> fresh
          vars1     = varE <$> fresh
          m         = varE (mkName "m")
          vars      = (appE [| pure |] <$> take i vars1)
                   ++ [appE m (vars1 !! i)]
                   ++ (appE [| pure |] <$> drop (i + 1) vars1)
          apps f as = foldl (\a (j, b) -> star j `appE` a `appE` b) f (zip [0 :: Int ..] as)
                      where star 0 = [| (<$>) |]
                            star _ = [| (<*>) |]

  keep con =
    case con of
      NormalC c fs  -> let s = take (length fs) in (conP c (s pats), apps (conE c) (s vars))
      RecC    c fs  -> let s = take (length fs) in (conP c (s pats), apps (conE c) (s vars))
      InfixC  _ c _ -> ( infixP (pats !! 0) c (pats !! 1)
                       , apps (conE c) (take 2 vars)
                       )
      ForallC _ _ c -> keep c
    where fresh     = mkName <$> delete "f" (delete "m" freshNames)
          pats      = varP <$> fresh
          vars      = varE <$> fresh
          apps f as = foldl appE f as

  fail_ con =
    case con of
      NormalC c fs  -> let s = take (length fs) in conP c (s pats)
      RecC    c fs  -> let s = take (length fs) in conP c (s pats)
      InfixC  _ c _ -> infixP (pats !! 0) c (pats !! 1)
      ForallC _ _ c -> fail_ c
    where pats = repeat wildP

freshNames :: [String]
freshNames = map pure ['a'..'z'] ++ map (('a':) . show) [0 :: Integer ..]

-------------------------------------------------------------------------------

computeTypes :: Bool -> Type -> Name -> [TyVarBndr] -> Subst -> Q Typing
computeTypes forcedMono fieldtype datatype dtVars_ subst =

  do let fieldVars = typeVariables fieldtype
         tyO       = return fieldtype
         dtTypes   = substitute subst . typeFromBinder <$> dtVars_
         dtBinders = concatMap binderFromType dtTypes
         varNames  = nameFromBinder <$> dtBinders
         usedVars  = filter (`elem` fieldVars) varNames
         tyI       = return $ foldr (flip AppT) (ConT datatype) (reverse dtTypes)
         pretties  = mapTyVarBndr pretty <$> dtBinders
         mono      = forcedMono || isMonomorphic fieldtype dtBinders

     if mono
       then return $ Typing
               mono
               (prettyType <$> tyI)
               (prettyType <$> tyO)
               (nub pretties)
       else
         do let names = return <$> ['a'..'z']
                used  = show . pretty <$> varNames
                free  = filter (not . (`elem` used)) names
            subs <- forM (zip usedVars free) (\(a, b) -> (,) a <$> newName b)
            let rename = mapTypeVariables (\a -> a `fromMaybe` lookup a subs)

            return $ Typing
              mono
              (prettyType <$> [t| $tyI -> $(rename <$> tyI) |])
              (prettyType <$> [t| $tyO -> $(rename <$> tyO) |])
              (nub (pretties ++ map (mapTyVarBndr pretty) (PlainTV . snd <$> subs)))

isMonomorphic :: Type -> [TyVarBndr] -> Bool
isMonomorphic field vars =
  let fieldVars = typeVariables field
      varNames  = nameFromBinder <$> vars
      usedVars  = filter (`elem` fieldVars) varNames
   in null usedVars

-------------------------------------------------------------------------------
-- Generic helper functions dealing with Template Haskell

typeVariables :: Type -> [Name]
typeVariables = map nameFromBinder . binderFromType

typeFromBinder :: TyVarBndr -> Type
typeFromBinder (PlainTV  tv     ) = VarT tv
typeFromBinder (KindedTV tv kind) = SigT (VarT tv) kind

binderFromType :: Type -> [TyVarBndr]
binderFromType = go
  where
  go ty =
    case ty of
      ForallT ts _ _ -> ts
      AppT a b       -> go a ++ go b
      SigT t _       -> go t
      VarT n         -> [PlainTV n]
      _              -> []

mapTypeVariables :: (Name -> Name) -> Type -> Type
mapTypeVariables f = go
  where
  go ty =
    case ty of
      ForallT ts a b -> ForallT (mapTyVarBndr f <$> ts) (mapPred f <$> a) (go b)
      AppT a b       -> AppT (go a) (go b)
      SigT t a       -> SigT (go t) a
      VarT n         -> VarT (f n)
      t              -> t

mapType :: (Type -> Type) -> Type -> Type
mapType f = go
  where
  go ty =
    case ty of
      ForallT v c t -> f (ForallT v c (go t))
      AppT a b      -> f (AppT (go a) (go b))
      SigT t k      -> f (SigT (go t) k)
      _             -> f ty

substitute :: Subst -> Type -> Type
substitute env = mapType sub
  where sub v = case lookup v env of
                  Nothing -> v
                  Just w  -> w

nameFromBinder :: TyVarBndr -> Name
nameFromBinder (PlainTV  n  ) = n
nameFromBinder (KindedTV n _) = n

mapPred :: (Name -> Name) -> Pred -> Pred
mapPred f (ClassP n ts) = ClassP (f n) (mapTypeVariables f <$> ts)
mapPred f (EqualP t x ) = EqualP (mapTypeVariables f t) (mapTypeVariables f x)

mapTyVarBndr :: (Name -> Name) -> TyVarBndr -> TyVarBndr
mapTyVarBndr f (PlainTV  n  ) = PlainTV (f n)
mapTyVarBndr f (KindedTV n a) = KindedTV (f n) a

-- Prettify a TH name.

pretty :: Name -> Name
pretty tv = mkName (takeWhile (/= '_') (show tv))

-- Prettify a type.

prettyType :: Type -> Type
prettyType = mapTypeVariables pretty

-- Throw a label specific error.

fclError :: String -> a
fclError err = error ("Label.Derive: " ++ err)

