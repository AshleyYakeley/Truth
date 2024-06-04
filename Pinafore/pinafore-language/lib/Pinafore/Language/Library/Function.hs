module Pinafore.Language.Library.Function
    ( functionLibSection
    ) where

import Import
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Types
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type
import Pinafore.Language.Var

revap :: A -> (A -> B) -> B
revap x f = f x

functionLibSection :: LibraryStuff context
functionLibSection =
    headingBDS
        "Function"
        ""
        [ typeBDS "->" "A pure function." (MkSomeGroundType funcGroundType) []
        , namespaceBDS "Function" $
          monadEntries @_ @((->) P) <>
          [ addNameInRootBDS $ valBDS "$" "Apply a function to a value." $ id @(->) @(A -> B)
          , addNameInRootBDS $ valBDS ">-" "Apply a value to a function." revap
          , addNameInRootBDS $ valBDS "id" "The identity function." $ id @(->) @A
          , addNameInRootBDS $ valBDS "." "Compose functions." $ (.) @(->) @A @B @C
          , addNameInRootBDS $ valBDS "fix" "Fixed point of a function." $ fix @A
          , addNameInRootBDS $ valBDS "error" "Error." $ ((\t -> error (unpack t)) :: Text -> BottomType)
          , addNameInRootBDS $ valBDS "undefined" "Same as `error \"undefined\"`." $ ((error "undefined") :: BottomType)
          , addNameInRootBDS $
            valBDS
                "seq"
                "Evaluate the first argument, then if that's not \"bottom\" (error or non-termination), return the second argument."
                (seq :: TopType -> A -> A)
          , addNameInRootBDS $
            specialFormBDS "check" "Check from a dynamic supertype." ["@A"] "D(A) -> Maybe A" $
            MkQSpecialForm (ConsListType AnnotNegativeType NilListType) $ \(MkSome tn, ()) -> do
                dtw <- getGreatestDynamicSupertype tn
                tpw <- invertType tn
                return $ MkSomeOf (funcShimWit dtw $ maybeShimWit tpw) id
          , addNameInRootBDS $
            specialFormBDS "coerce" "Coerce from a dynamic supertype." ["@A"] "D(A) -> A" $
            MkQSpecialForm (ConsListType AnnotNegativeType NilListType) $ \(MkSome tn, ()) -> do
                dtw <- getGreatestDynamicSupertype tn
                tpw <- invertType tn
                return $
                    MkSomeOf (funcShimWit dtw tpw) $ \case
                        Just t -> t
                        Nothing ->
                            error $
                            unpack $ toText $ "coercion from " <> exprShow dtw <> " to " <> exprShow tn <> " failed"
          ]
        ]
