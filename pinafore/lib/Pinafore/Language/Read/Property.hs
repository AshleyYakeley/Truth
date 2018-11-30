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
       forall baseedit. HasPinaforeEntityEdit baseedit
    => Parser (PinaforeTypeCheck (QExpr baseedit))
readProperty = do
    readThis TokProperty
    readThis TokAt
    ceta <- readEntityType3
    readThis TokAt
    cetb <- readEntityType3
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
                           morphism = propertyMorphism (entityAdapter eta) (entityAdapter etb) (MkPredicate uuid)
                           pinamorphism = MkPinaforeMorphism pra prb morphism
                           anyval = toTypeFAnyValue typef pinamorphism
                           in return $ qConstExprAny anyval
