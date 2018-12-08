module Pinafore.Language.Type.Simplify.FreeVars
    ( mergeFreeExpressionTermVars
    ) where

import Language.Expression.Dolan
import Language.Expression.Expression
import Language.Expression.Named
import Language.Expression.Sealed
import Pinafore.Language.Type.Subtype
import Pinafore.Language.Type.Type
import Pinafore.Language.Type.Unify
import Shapes

subtypePositiveContext :: SubtypeContext baseedit (PinaforeUnifierMonad baseedit) 'PositivePolarity 'PositivePolarity
subtypePositiveContext = let
    subtypeLift = lift
    subtypeTypes = subtypePositiveType
    subtypeInverted = subtypeNegativeContext
    in MkSubtypeContext {..}

subtypePositiveSingularType ::
       PinaforeSingularType baseedit 'PositivePolarity a
    -> PinaforeSingularType baseedit 'PositivePolarity b
    -> PinaforeUnifierMonad baseedit (a -> b)
subtypePositiveSingularType (VarPinaforeSingularType va) (VarPinaforeSingularType vb)
    | Just Refl <- testEquality va vb = return id
subtypePositiveSingularType (GroundPinaforeSingularType ga argsa) (GroundPinaforeSingularType gb argsb) =
    subtypeGroundTypes subtypePositiveContext ga argsa gb argsb
subtypePositiveSingularType _ _ = empty

subtypePositiveType1 ::
       PinaforeType baseedit 'PositivePolarity a
    -> PinaforeSingularType baseedit 'PositivePolarity b
    -> PinaforeUnifierMonad baseedit (a -> b)
subtypePositiveType1 NilPinaforeType _ = return never
subtypePositiveType1 (ConsPinaforeType t1 tr) tb = do
    conv1 <- subtypePositiveSingularType t1 tb
    convr <- subtypePositiveType1 tr tb
    return $ joinf conv1 convr

subtypePositiveType ::
       PinaforeType baseedit 'PositivePolarity a
    -> PinaforeType baseedit 'PositivePolarity b
    -> PinaforeUnifierMonad baseedit (a -> b)
subtypePositiveType _ NilPinaforeType = empty
subtypePositiveType ta (ConsPinaforeType t1 tr) =
    (fmap (\conv -> join1 . conv) $ subtypePositiveType1 ta t1) <|>
    (fmap (\conv -> join2 . conv) $ subtypePositiveType ta tr)

subtypeNegativeContext :: SubtypeContext baseedit (PinaforeUnifierMonad baseedit) 'NegativePolarity 'NegativePolarity
subtypeNegativeContext = let
    subtypeLift = lift
    subtypeTypes = subtypeNegativeType
    subtypeInverted = subtypePositiveContext
    in MkSubtypeContext {..}

subtypeNegativeSingularType ::
       PinaforeSingularType baseedit 'NegativePolarity a
    -> PinaforeSingularType baseedit 'NegativePolarity b
    -> PinaforeUnifierMonad baseedit (a -> b)
subtypeNegativeSingularType (VarPinaforeSingularType va) (VarPinaforeSingularType vb)
    | Just Refl <- testEquality va vb = return id
subtypeNegativeSingularType (GroundPinaforeSingularType ga argsa) (GroundPinaforeSingularType gb argsb) =
    subtypeGroundTypes subtypeNegativeContext ga argsa gb argsb
subtypeNegativeSingularType _ _ = empty

subtypeNegativeType1 ::
       PinaforeType baseedit 'NegativePolarity a
    -> PinaforeSingularType baseedit 'NegativePolarity b
    -> PinaforeUnifierMonad baseedit (a -> b)
subtypeNegativeType1 NilPinaforeType _ = empty
subtypeNegativeType1 (ConsPinaforeType t1 tr) tb =
    (fmap (\conv -> conv . meet1) $ subtypeNegativeSingularType t1 tb) <|>
    (fmap (\conv -> conv . meet2) $ subtypeNegativeType1 tr tb)

subtypeNegativeType ::
       PinaforeType baseedit 'NegativePolarity a
    -> PinaforeType baseedit 'NegativePolarity b
    -> PinaforeUnifierMonad baseedit (a -> b)
subtypeNegativeType _ NilPinaforeType = return alwaysTop
subtypeNegativeType ta (ConsPinaforeType t1 tr) = do
    conv1 <- subtypeNegativeType1 ta t1
    convr <- subtypeNegativeType ta tr
    return $ meetf conv1 convr

matchWits ::
       Eq name
    => NameWitness name (PinaforeType baseedit 'NegativePolarity) a
    -> NameWitness name (PinaforeType baseedit 'NegativePolarity) b
    -> PinaforeUnifierMonad baseedit (a -> b)
matchWits (MkNameWitness na ta) (MkNameWitness nb tb)
    | na == nb = subtypeNegativeType ta tb
matchWits _ _ = empty

mergeFreeTermVarM ::
       Eq name
    => NameWitness name (PinaforeType baseedit 'NegativePolarity) a
    -> NamedExpression name (PinaforeType baseedit 'NegativePolarity) (a -> b)
    -> PinaforeUnifierMonad baseedit (NamedExpression name (PinaforeType baseedit 'NegativePolarity) b)
mergeFreeTermVarM _ (ClosedExpression _) = empty
mergeFreeTermVarM wit (OpenExpression wite expr) =
    (do
         conv <- matchWits wit wite
         return $ OpenExpression wit $ fmap (\tab a -> tab (conv a) a) expr) <|>
    (do
         conv <- matchWits wite wit
         return $ OpenExpression wite $ fmap (\tab t -> tab t $ conv t) expr) <|>
    (do
         expr' <- mergeFreeTermVarM wit $ fmap (\tab a t -> tab t a) expr
         return $ OpenExpression wite expr')

mergeFreeTermVar ::
       Eq name
    => NameWitness name (PinaforeType baseedit 'NegativePolarity) a
    -> NamedExpression name (PinaforeType baseedit 'NegativePolarity) (a -> b)
    -> PinaforeUnifierMonad baseedit (NamedExpression name (PinaforeType baseedit 'NegativePolarity) b)
mergeFreeTermVar wit expr = mergeFreeTermVarM wit expr <|> return (OpenExpression wit expr)

mergeFreeTermVars ::
       Eq name
    => NamedExpression name (PinaforeType baseedit 'NegativePolarity) t
    -> PinaforeUnifierMonad baseedit (NamedExpression name (PinaforeType baseedit 'NegativePolarity) t)
mergeFreeTermVars (ClosedExpression v) = return $ ClosedExpression v
mergeFreeTermVars (OpenExpression wit expr) = do
    expr' <- mergeFreeTermVars expr
    mergeFreeTermVar wit expr'

mergeFreeExpressionTermVars ::
       PinaforeExpression baseedit -> PinaforeUnifierMonad baseedit (PinaforeExpression baseedit)
mergeFreeExpressionTermVars (MkSealedExpression t expr) = do
    expr' <- mergeFreeTermVars $ expr
    return $ MkSealedExpression t expr'
