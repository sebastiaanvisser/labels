-- | Template Haskell functions for automatically generating labels for
-- algebraic datatypes, newtypes and GADTs. There are two basic modes of label
-- generation, the `mkLabels` family of functions create labels (and optionally
-- type signatures) in scope as top level funtions, and the `getLabel` family
-- of funtions create labels as expressions that can be named and typed
-- manually.

-- In the case of multi-constructor datatypes some fields might not always be
-- available and the getter and/or modifier for the derived labels will be
-- partial. Partial labels are provided with an additional partial type context
-- that forces them to be only usable in some `Alternative` context.

{-# LANGUAGE TemplateHaskell #-}

module Label.Derive
(

-- * First class record labels.
  label

-- * Generate labels in scope.
, mkLabel
, mkLabels

-- * Produce labels as expressions.
, getLabel

-- * Low level derivation functions.
, mkLabelsWith
, getLabelWith
, defaultNaming
)
where

import Control.Applicative
import Control.Monad
import Data.Char (toLower, toUpper)
import Language.Haskell.TH

import Label.Codegen

-- | Derive labels for all the record types in the supplied declaration. The
-- record fields don't need an underscore prefix. Multiple data types /
-- newtypes are allowed at once.
--
-- The advantage of this approach is that you don't need to explicitly hide the
-- original record accessors from being exported and they won't show up in the
-- derived `Show` instance.
--
-- Example:
--
-- > label [d|
-- >   data Record = Record
-- >     { int  :: Int
-- >     , bool :: Bool
-- >     } deriving Show
-- >   |]
--
-- > ghci> modify int (+2) (Record 1 False)
-- > Record 3 False

label :: Q [Dec] -> Q [Dec]
label decls =
  do ds <- decls
     ls <- forM (ds >>= lbls) (labelDeclaration id True False)
     return (concat ((delabelize <$> ds) : ls))
  where

  lbls :: Dec -> [Dec]
  lbls dec =
    case dec of
      DataD    {} -> [dec]
      NewtypeD {} -> [dec]
      _           -> []

  delabelize :: Dec -> Dec
  delabelize dec =
    case dec of
      DataD    ctx nm vars cs ns -> DataD    ctx nm vars (con <$> cs) ns
      NewtypeD ctx nm vars c  ns -> NewtypeD ctx nm vars (con c)      ns
      rest                       -> rest
    where con (RecC n vst) = NormalC n (map (\(_, s, t) -> (s, t)) vst)
          con c            = c

-- | Derive labels including type signatures for all the record selectors in a
-- single datatype. The types will be polymorphic and can be used in an
-- arbitrary context.

mkLabel :: Name -> Q [Dec]
mkLabel = mkLabels . return

-- | Derive labels including type signatures for all the record selectors for a
-- collection of datatypes. The types will be polymorphic and can be used in an
-- arbitrary context.

mkLabels :: [Name] -> Q [Dec]
mkLabels = liftM concat . mapM (mkLabelsWith defaultNaming True True)

-- | Derive unnamed labels as n-tuples that can be named manually. The types
-- will be polymorphic and can be used in an arbitrary context.
--
-- Example:
--
-- > (left, right) = $(getLabel ''Either)
--
-- The labels can now also be typed manually:
--
-- > left  :: (Either a b -> Either c b) :~> (a -> c)
-- > right :: (Either a b -> Either a c) :~> (b -> c)
--
-- Note: Because of the abstract nature of the generated labels and the top
-- level pattern match, it might be required to use 'NoMonomorphismRestriction'
-- in some cases.

getLabel :: Name -> Q Exp
getLabel = getLabelWith True

-------------------------------------------------------------------------------

-- | Low level label as expression derivation function.

getLabelWith
  :: Bool  -- ^ Generate type signatures or not.
  -> Name  -- ^ The type to derive labels for.
  -> Q Exp

getLabelWith sigs name =
  do dec    <- reifyDec name
     labels <- generateLabels id dec
     let bodies  =        map (\(LabelExpr _ _ _ b) -> b) labels
         types   =        map (\(LabelExpr _ _ t _) -> t) labels
         context = head $ map (\(LabelExpr _ c _ _) -> c) labels
         vars    = head $ map (\(LabelExpr v _ _ _) -> v) labels
     if sigs
       then tupE bodies `sigE`
              forallT vars context (foldl appT (tupleT (length bodies)) types)
       else tupE bodies

-- | Low level standalone label derivation function.

mkLabelsWith
  :: (String -> String) -- ^ Supply a function to perform custom label naming.
  -> Bool               -- ^ Generate type signatures or not.
  -> Bool               -- ^ Generate inline pragma or not.
  -> Name               -- ^ The type to derive labels for.
  -> Q [Dec]

mkLabelsWith mk sigs inl name =
  do dec <- reifyDec name
     labelDeclaration mk sigs inl dec

-- | Default way of generating a label name from the Haskell record selector
-- name. If the original selector starts with an underscore, remove it and make
-- the next character lowercase. Otherwise, add 'l', and make the next
-- character uppercase.

defaultNaming :: String -> String
defaultNaming field =
  case field of
    '_' : c : rest -> toLower c : rest
    f : rest       -> 'l' : toUpper f : rest
    n              -> fclError ("Cannot derive label for record selector with name: " ++ n)

-------------------------------------------------------------------------------

-- Reify a name into a declaration.

reifyDec :: Name -> Q Dec
reifyDec name =
  do info <- reify name
     case info of
       TyConI dec -> return dec
       _ -> fclError "Info must be type declaration type."

-- Throw a label specific error.

fclError :: String -> a
fclError err = error ("Label.Derive: " ++ err)

