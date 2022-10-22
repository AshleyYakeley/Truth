module Pinafore.Language.Library.Eval
    ( evalLibraryModule
    ) where

import Pinafore.Base
import Pinafore.Language.Convert.Types
import Pinafore.Language.DocTree
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Convert ()
import Pinafore.Language.Library.Std.Types
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type
import Shapes

evalLibraryModule :: LibraryModule context
evalLibraryModule =
    MkLibraryModule $
    MkDocTree
        "Eval"
        ""
        [ mkSpecialFormEntry
              "evaluate"
              "A function that evaluates text as a Pinafore expression to be subsumed to positive type `A`.\n\n\
                \The result of the action is either the value (`Right`), or an error message (`Left`).\n\n\
                \The local scope is not in any way transmitted to the evaluation."
              ["@A"]
              "Text -> Action (Either Text A)" $
          MkSpecialForm (ConsListType AnnotPositiveType NilListType) $ \(MkSome tp, ()) -> do
              spvals <- getSpecialVals
              let
                  valShimWit :: forall t. QShimWit 'Positive t -> QShimWit 'Positive (Text -> Action (Either Text t))
                  valShimWit t' = funcShimWit textShimWit $ actionShimWit $ eitherShimWit textShimWit t'
              return $ MkSomeOf (valShimWit $ mkPolarShimWit tp) $ specialEvaluate spvals tp
        ]
