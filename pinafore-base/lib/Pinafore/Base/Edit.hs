module Pinafore.Base.Edit
    ( Predicate(..)
    , PinaforeEntityRead(..)
    , PinaforeEntityEdit(..)
    , PinaforeEntityUpdate
    , HasPinaforeEntityUpdate
    ) where

import Data.Aeson (FromJSON)
import Pinafore.Base.Entity
import Pinafore.Base.Know
import Pinafore.Base.Lens
import Pinafore.Base.Literal
import Shapes
import Truth.Core

newtype Predicate =
    MkPredicate Anchor
    deriving (Eq, FromJSON, Show)

-- | Some of these reads may add to the database, but will always give consistent results between changes.
data PinaforeEntityRead t where
    PinaforeEntityReadGetPredicate :: Predicate -> Entity -> PinaforeEntityRead (Know Entity)
    PinaforeEntityReadGetProperty :: Predicate -> Entity -> PinaforeEntityRead Entity
    PinaforeEntityReadLookupPredicate :: Predicate -> Entity -> PinaforeEntityRead (FiniteSet Entity)
    PinaforeEntityReadToLiteral :: Entity -> PinaforeEntityRead (Know Literal)

instance Show (PinaforeEntityRead t) where
    show (PinaforeEntityReadGetPredicate p s) = "get " ++ show p ++ " of " ++ show s
    show (PinaforeEntityReadGetProperty p s) = "get prop " ++ show p ++ " of " ++ show s
    show (PinaforeEntityReadLookupPredicate p v) = "lookup " ++ show p ++ " for " ++ show v
    show (PinaforeEntityReadToLiteral v) = "to literal " ++ show v

instance AllWitnessConstraint Show PinaforeEntityRead where
    allWitnessConstraint = Dict

instance WitnessConstraint Show PinaforeEntityRead where
    witnessConstraint (PinaforeEntityReadGetPredicate _ _) = Dict
    witnessConstraint (PinaforeEntityReadGetProperty _ _) = Dict
    witnessConstraint (PinaforeEntityReadLookupPredicate _ _) = Dict
    witnessConstraint (PinaforeEntityReadToLiteral _) = Dict

data PinaforeEntityEdit where
    PinaforeEntityEditSetPredicate :: Predicate -> Entity -> Know Entity -> PinaforeEntityEdit -- pred subj kval
    PinaforeEntityEditSetLiteral :: Entity -> Know Literal -> PinaforeEntityEdit

instance Floating PinaforeEntityEdit PinaforeEntityEdit

instance ApplicableEdit PinaforeEntityEdit where
    applyEdit (PinaforeEntityEditSetPredicate p s kv) _ (PinaforeEntityReadGetPredicate p' s')
        | p == p'
        , s == s' = return kv
    applyEdit (PinaforeEntityEditSetPredicate p s (Known v)) _ (PinaforeEntityReadGetProperty p' s')
        | p == p'
        , s == s' = return v
    applyEdit (PinaforeEntityEditSetPredicate p s Unknown) _ (PinaforeEntityReadGetProperty p' s')
        | p == p'
        , s == s' = newEntity
    applyEdit (PinaforeEntityEditSetPredicate p s (Known v)) mr (PinaforeEntityReadLookupPredicate p' v')
        | p == p'
        , v == v' = do
            ss <- mr $ PinaforeEntityReadLookupPredicate p' v'
            return $ insertSet s ss
    applyEdit (PinaforeEntityEditSetPredicate p s Unknown) mr (PinaforeEntityReadLookupPredicate p' v')
        | p == p' = do
            ss <- mr $ PinaforeEntityReadLookupPredicate p' v'
            return $ deleteSet s ss
    applyEdit (PinaforeEntityEditSetLiteral s kl) _ (PinaforeEntityReadToLiteral s')
        | s == s' = return kl
    applyEdit _ mr rt = mr rt

instance InvertibleEdit PinaforeEntityEdit where
    invertEdit (PinaforeEntityEditSetPredicate p s kv) mr = do
        kv' <- mr $ PinaforeEntityReadGetPredicate p s
        return $
            if kv == kv'
                then []
                else [PinaforeEntityEditSetPredicate p s kv']
    invertEdit (PinaforeEntityEditSetLiteral s kl) mr = do
        kl' <- mr $ PinaforeEntityReadToLiteral s
        return $
            if kl == kl'
                then []
                else [PinaforeEntityEditSetLiteral s kl']

type instance EditReader PinaforeEntityEdit = PinaforeEntityRead

instance Show PinaforeEntityEdit where
    show (PinaforeEntityEditSetPredicate p s kv) = "set " ++ show p ++ " of " ++ show s ++ " to " ++ show kv
    show (PinaforeEntityEditSetLiteral p kl) = "set " ++ show p ++ " to " ++ show kl

type PinaforeEntityUpdate = EditUpdate PinaforeEntityEdit

type HasPinaforeEntityUpdate = BaseChangeLens PinaforeEntityUpdate

instance BaseChangeLens PinaforeEntityUpdate PinaforeEntityUpdate where
    baseChangeLens = id
