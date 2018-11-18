module Pinafore.Language.Read.Property
    ( readProperty
    ) where

import Pinafore.Base
import Pinafore.Language.Expression
import Pinafore.Language.Literal
import Pinafore.Language.Morphism
import Pinafore.Language.NamedEntity
import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.Token
import Pinafore.Language.Read.Type
import Pinafore.Language.Type
import Shapes hiding (try)

type PinaforeRangeF baseedit t = AnyF (RangeType (PinaforeType baseedit) 'PositivePolarity) (Range t)

literalRangeF :: forall baseedit t. LiteralType t -> PinaforeRangeF baseedit t
literalRangeF lt = let
    tfp :: TypeF (PinaforeType baseedit) 'NegativePolarity t
    tfp =
        singlePinaforeTypeF $
        mkTypeF $
        GroundPinaforeSingularType (SimpleEntityPinaforeGroundType $ LiteralSimpleEntityType lt) NilDolanArguments
    tfq :: TypeF (PinaforeType baseedit) 'PositivePolarity t
    tfq =
        singlePinaforeTypeF $
        mkTypeF $
        GroundPinaforeSingularType (SimpleEntityPinaforeGroundType $ LiteralSimpleEntityType lt) NilDolanArguments
    in biTypeF (tfp, tfq)

entityRangeF :: forall baseedit name. SymbolWitness name -> PinaforeRangeF baseedit Point
entityRangeF MkSymbolWitness = let
    tfp :: TypeF (PinaforeType baseedit) 'NegativePolarity (NamedEntity name)
    tfp = fromTypeF
    tfq :: TypeF (PinaforeType baseedit) 'PositivePolarity (NamedEntity name)
    tfq = toTypeF
    in biTypeF (fmap unNamedEntity tfp, contramap MkNamedEntity tfq)

readPropertyTypeA :: Parser (PinaforeTypeCheck (PinaforeRangeF baseedit Point))
readPropertyTypeA = do
    n <- readTypeName
    return $ do
        nt <- lookupNamedType n
        case nt of
            EntityNamedType (MkAnyW sw) -> return $ entityRangeF sw

data MorphismCodomain baseedit
    = PointMorphismCodomain (PinaforeRangeF baseedit Point)
    | forall t. AsLiteral t => LiteralMorphismCodomain (PinaforeRangeF baseedit t)

readPropertyTypeB :: Parser (PinaforeTypeCheck (MorphismCodomain baseedit))
readPropertyTypeB =
    (do
         MkAnyW lt <- readLiteralType
         case literalTypeAsLiteral lt of
             Dict -> return $ return $ LiteralMorphismCodomain $ literalRangeF lt) <|>
    (do
         crfp <- readPropertyTypeA
         return $ do
             rfp <- crfp
             return $ PointMorphismCodomain rfp)

readProperty ::
       forall baseedit. HasPinaforePointEdit baseedit
    => Parser (PinaforeTypeCheck (QExpr baseedit))
readProperty = do
    readThis TokProperty
    readThis TokAt
    crfpa <- readPropertyTypeA @baseedit
    readThis TokAt
    crfplb <- readPropertyTypeB @baseedit
    uuid <- readThis TokAnchor
    return $ do
        MkAnyF rta pra <- crfpa
        rfplb <- crfplb
        case rfplb of
            PointMorphismCodomain (MkAnyF rtb prb) ->
                withSubrepresentative rangeTypeInKind rta $
                withSubrepresentative rangeTypeInKind rtb $
                return $
                qConstExprAny $
                toTypeFAnyValue
                    (singlePinaforeTypeF $
                     mkTypeF $
                     GroundPinaforeSingularType MorphismPinaforeGroundType $
                     ConsDolanArguments rta $ ConsDolanArguments rtb NilDolanArguments) $
                MkPinaforeMorphism pra prb $ predicatePinaforeLensMorphism (MkPredicate uuid)
            LiteralMorphismCodomain (MkAnyF rtb ltb) ->
                withSubrepresentative rangeTypeInKind rta $
                withSubrepresentative rangeTypeInKind rtb $
                return $
                qConstExprAny $
                toTypeFAnyValue
                    (singlePinaforeTypeF $
                     mkTypeF $
                     GroundPinaforeSingularType MorphismPinaforeGroundType $
                     ConsDolanArguments rta $ ConsDolanArguments rtb NilDolanArguments) $
                MkPinaforeMorphism pra ltb $
                literalPinaforeLensMorphism . predicatePinaforeLensMorphism (MkPredicate uuid)
