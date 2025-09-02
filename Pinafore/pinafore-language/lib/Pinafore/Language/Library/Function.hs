module Pinafore.Language.Library.Function
    ( functionLibSection
    )
where

import Import
import Pinafore.Language.Convert.Pinafore
import Pinafore.Language.Convert.Var
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Types
import Pinafore.Language.Type

revap :: A -> (A -> B) -> B
revap x f = f x

langCheck :: LangType -> QInterpreter LangExpression
langCheck (MkLangType npt) = do
    let
        tn = nonpolarToNegative @QTypeSystem npt
        tp = nonpolarToPositive @QTypeSystem npt
    gdssw <- getGreatestDynamicSupertypeSW tn
    return $ case gdssw of
        MkShimWit dtw (MkPolarShim (MkPolyComposeShim expr)) ->
            let
                stype = funcShimWit (mkShimWit dtw) $ maybeShimWit tp
                sexpr = fmap shimToFunction expr
                in MkLangExpression $ MkSealedExpression stype sexpr

langCoerce :: LangType -> QInterpreter LangExpression
langCoerce (MkLangType npt) = do
    let
        tn = nonpolarToNegative @QTypeSystem npt
        tp = nonpolarToPositive @QTypeSystem npt
    gdssw <- getGreatestDynamicSupertypeSW tn
    return $ case gdssw of
        MkShimWit dtw (MkPolarShim (MkPolyComposeShim expr)) ->
            let
                fromJustOrError :: forall t. Maybe t -> t
                fromJustOrError =
                    \case
                        Just t -> t
                        Nothing ->
                            error $ unpack $ toText $ "coercion from " <> exprShow dtw <> " to " <> exprShow tn <> " failed"
                stype = funcShimWit (mkShimWit dtw) tp
                sexpr = fmap (\conv t -> fromJustOrError $ shimToFunction conv t) expr
                in MkLangExpression $ MkSealedExpression stype sexpr

functionLibSection :: LibraryStuff
functionLibSection =
    headingBDS
        "Function"
        ""
        [ typeBDS "->" "A pure function." (MkSomeGroundType funcGroundType) []
        , namespaceBDS "Function"
            $ monadEntries @((->) P)
            <> [ addNameInRootBDS $ valBDS "$" "Apply a function to a value." $ id @(->) @(A -> B)
               , addNameInRootBDS $ valBDS ">-" "Apply a value to a function." revap
               , addNameInRootBDS $ valBDS "id" "The identity function." $ id @(->) @A
               , addNameInRootBDS $ valBDS "." "Compose functions." $ (.) @(->) @A @B @C
               , addNameInRootBDS $ valBDS "fix" "Fixed point of a function." $ fix @A
               , addNameInRootBDS $ valBDS "error" "Error." $ ((\t -> error (unpack t)) :: Text -> BottomType)
               , addNameInRootBDS $ valBDS "undefined" "Same as `error \"undefined\"`." $ ((error "undefined") :: BottomType)
               , addNameInRootBDS
                    $ valBDS
                        "seq"
                        "Evaluate the first argument, then if that's not \"bottom\" (error or non-termination), return the second argument."
                        (seq :: TopType -> A -> A)
               , addNameInRootBDS
                    $ valBDS "check" "`!{check @A}: D(A) -> Maybe A`  \nCheck from a dynamic supertype." langCheck
               , addNameInRootBDS
                    $ valBDS "coerce" "`!{coerce @A}: D(A) -> A`  \nCoerce from a dynamic supertype." langCoerce
               ]
        ]
