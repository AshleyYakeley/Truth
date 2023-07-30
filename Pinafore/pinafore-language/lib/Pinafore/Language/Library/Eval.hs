module Pinafore.Language.Library.Eval
    ( evalLibSection
    ) where

import Pinafore.Base
import Pinafore.Language.Convert.Types
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Types
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type
import Shapes

evalLibSection :: BindDocStuff context
evalLibSection =
    headingBDS "Eval" "" $
    pure $
    namespaceBDS
        "Eval"
        [ specialFormBDS
              "evaluate"
              "A function that evaluates text as a Pinafore expression to be subsumed to positive type `A`.\n\n\
                \The result of the action is either the value (`Right`), or an error message (`Left`).\n\n\
                \The local scope is not in any way transmitted to the evaluation."
              ["@A"]
              "Text -> Action (Either Text A)" $
          MkQSpecialForm (ConsListType AnnotPositiveType NilListType) $ \(MkSome tp, ()) -> do
              spvals <- getSpecialVals
              let
                  valShimWit :: forall t. QShimWit 'Positive t -> QShimWit 'Positive (Text -> Action (Either Text t))
                  valShimWit t' = funcShimWit textShimWit $ actionShimWit $ eitherShimWit textShimWit t'
              return $ MkSomeOf (valShimWit $ mkPolarShimWit tp) $ specialEvaluate spvals tp
        ]
