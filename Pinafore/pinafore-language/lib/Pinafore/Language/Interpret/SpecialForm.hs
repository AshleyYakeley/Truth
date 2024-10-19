module Pinafore.Language.Interpret.SpecialForm
    ( interpretSpecialForm
    ) where

import Import
import Pinafore.Language.Error
import Pinafore.Language.Interpret.Type
import Pinafore.Language.Interpreter
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type

specialFormArg :: QAnnotation t -> SyntaxAnnotation -> ComposeInner Maybe QInterpreter t
specialFormArg AnnotAnchor (SAAnchor anchor) = return anchor
specialFormArg AnnotType (SAType st) = lift $ interpretNonpolarType st
specialFormArg _ _ = liftInner Nothing

specialFormArgs :: ListType QAnnotation lt -> [SyntaxAnnotation] -> ComposeInner Maybe QInterpreter (ListProduct lt)
specialFormArgs NilListType [] = return ()
specialFormArgs (ConsListType t tt) (a:aa) = do
    v <- specialFormArg t a
    vv <- specialFormArgs tt aa
    return (v, vv)
specialFormArgs _ _ = liftInner Nothing

showSA :: SyntaxAnnotation -> NamedText
showSA (SAType _) = "type"
showSA (SAAnchor _) = "anchor"

showAnnotation :: QAnnotation a -> NamedText
showAnnotation AnnotAnchor = "anchor"
showAnnotation AnnotType = "type"

interpretSpecialForm :: FullNameRef -> Maybe QSpecialForm -> [SyntaxAnnotation] -> QInterpreter QExpression
interpretSpecialForm name msf annotations = do
    MkQSpecialForm largs val <-
        case msf of
            Just sf -> return sf
            Nothing -> lookupSpecialForm name
    margs <- unComposeInner $ specialFormArgs largs annotations
    case margs of
        Just args -> val args
        Nothing ->
            throw $
            SpecialFormWrongAnnotationsError name (listTypeToList showAnnotation largs) (fmap showSA annotations)
