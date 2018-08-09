module Language.Expression.Bindings
    ( Bindings
    , singleBinding
    , bindingsNames
    , bindingsDuplicates
    , bindingsCheckDuplicates
    , bindingsLetExpression
    , bindingsLetSealedExpression
    ) where

import Data.List (nub)
import Language.Expression.NameWit
import Language.Expression.Sealed
import Shapes

data Binding nw vw tw where
    MkBinding :: nw n -> SealedExpression nw vw tw -> Binding nw vw tw

newtype Bindings nw vw tw =
    MkBindings [Binding nw vw tw]
    deriving (Semigroup, Monoid)

singleBinding :: nw n -> SealedExpression nw vw tw -> Bindings nw vw tw
singleBinding name expr = MkBindings $ pure $ MkBinding name expr

data Bound m nw vw =
    forall vals. MkBound (forall a. NameTypeExpression nw vw a -> m (NameTypeExpression nw vw (vals -> a)))
                         (NameTypeExpression nw vw vals)

mkBound ::
       (TestEquality nw, Monad m)
    => (forall n. nw n -> TypeJoiner m (vw n))
    -> (forall n. nw n -> TypeChecker m tw (vw n))
    -> [Binding nw vw tw]
    -> Bound m nw vw
mkBound _ _ [] = MkBound (\e -> return $ fmap (\a _ -> a) e) (pure ())
mkBound joiner checker ((MkBinding name (MkSealedExpression twt expr)):bb) =
    case mkBound joiner checker bb of
        MkBound abstractNames exprs ->
            MkBound
                (\e -> do
                     e' <- abstractNames e
                     abstractNTExpression (joiner name) name e' $ \vwnt e'' ->
                         case checker name of
                             MkTypeChecker ch -> do
                                 conv <- ch twt vwnt
                                 return $ fmap (\t'va ~(t, vals) -> t'va (conv t) vals) e'')
                ((,) <$> expr <*> exprs)

bindingsLetExpression ::
       (TestEquality nw, Monad m)
    => (forall n. nw n -> TypeJoiner m (vw n))
    -> (forall n. nw n -> TypeChecker m tw (vw n))
    -> Bindings nw vw tw
    -> NameTypeExpression nw vw a
    -> m (NameTypeExpression nw vw a)
bindingsLetExpression joiner checker (MkBindings bb) body =
    case mkBound joiner checker bb of
        MkBound abstractNames exprs -> do
            exprsf <- abstractNames exprs
            bodyf <- abstractNames body
            return $ bodyf <*> fmap fix exprsf

bindingsLetSealedExpression ::
       (TestEquality nw, Monad m)
    => (forall n. nw n -> TypeJoiner m (vw n))
    -> (forall n. nw n -> TypeChecker m tw (vw n))
    -> Bindings nw vw tw
    -> SealedExpression nw vw tw
    -> m (SealedExpression nw vw tw)
bindingsLetSealedExpression joiner checker bindings (MkSealedExpression twt expr) = do
    expr' <- bindingsLetExpression joiner checker bindings expr
    return $ MkSealedExpression twt expr'

duplicates ::
       forall a. Eq a
    => [a]
    -> [a]
duplicates [] = []
duplicates (a:aa)
    | elem a aa = a : duplicates aa
duplicates (_:aa) = duplicates aa

bindingsNames :: Bindings nw vw tw -> [AnyWitness nw]
bindingsNames (MkBindings bb) = fmap (\(MkBinding name _) -> MkAnyWitness name) bb

bindingsDuplicates :: TestEquality nw => Bindings nw vw tw -> [AnyWitness nw]
bindingsDuplicates bindings = nub $ duplicates $ bindingsNames bindings

bindingsCheckDuplicates :: (AllWitnessConstraint Show nw, TestEquality nw, MonadFail m) => Bindings nw vw tw -> m ()
bindingsCheckDuplicates bindings =
    case bindingsDuplicates bindings of
        [] -> return ()
        b -> fail $ "duplicate bindings: " <> (intercalate ", " $ fmap show b)
