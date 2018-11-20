module Pinafore.Language.Read.Property
    ( readProperty
    ) where

import Pinafore.Base
import Pinafore.Language.EntityType
import Pinafore.Language.Expression
import Pinafore.Language.Morphism
import Pinafore.Language.Read.Parser
import Pinafore.Language.Read.Token
import Pinafore.Language.Read.Type
import Pinafore.Language.Type
import Shapes hiding (try)

readProperty ::
       forall baseedit. HasPinaforePointEdit baseedit
    => Parser (PinaforeTypeCheck (QExpr baseedit))
readProperty = do
    readThis TokProperty
    readThis TokAt
    ceta <- readEntityType
    readThis TokAt
    cetb <- readEntityType
    uuid <- readThis TokAnchor
    return $ do
        MkAnyW eta <- ceta
        MkAnyW etb <- cetb
        let
            bta = biTypeF (entityTypeToType eta, entityTypeToType eta)
            btb = biTypeF (entityTypeToType etb, entityTypeToType etb)
            in case (bta, btb, entityTypeEq eta, entityTypeEq etb) of
                   (MkAnyF rta pra, MkAnyF rtb prb, Dict, Dict) ->
                       withSubrepresentative rangeTypeInKind rta $
                       withSubrepresentative rangeTypeInKind rtb $ let
                           typef =
                               singlePinaforeTypeF $
                               mkTypeF $
                               GroundPinaforeSingularType MorphismPinaforeGroundType $
                               ConsDolanArguments rta $ ConsDolanArguments rtb NilDolanArguments
                           backMorphism = constructorAdapterBackwardMorphism $ entityPointAdapter eta
                           frontMorphism = constructorAdapterForwardMorphism $ entityPointAdapter etb
                           morphism = frontMorphism . predicatePinaforeLensMorphism (MkPredicate uuid) . backMorphism
                           pinamorphism = MkPinaforeMorphism pra prb morphism
                           anyval = toTypeFAnyValue typef pinamorphism
                           in return $ qConstExprAny anyval
