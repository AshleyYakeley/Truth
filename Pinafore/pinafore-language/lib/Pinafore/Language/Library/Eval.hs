module Pinafore.Language.Library.Eval
    ( evalLibSection
    ) where

import Import
import Pinafore.Language.Convert.Types
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Types
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type

evalLibSection :: LibraryStuff context
evalLibSection =
    headingBDS "Eval" "" $
    pure $
    namespaceBDS
        "Eval"
        [ specialFormBDS
              "evaluate"
              "A function that evaluates text as a Pinafore expression to be subsumed to positive type `A`.\n\n\
                \The local scope is not in any way transmitted to the evaluation."
              ["@A"]
              "Text -> Action (Result Text A)" $
          MkQSpecialForm (ConsListType AnnotPositiveType NilListType) $ \(MkSome (tp :: _ t), ()) -> do
              spvals <- getSpecialVals
              let
                  stype :: QShimWit 'Positive (Text -> Action (Result Text t))
                  stype = funcShimWit textShimWit $ actionShimWit $ resultShimWit textShimWit $ mkPolarShimWit tp
                  sval :: Text -> Action (Result Text t)
                  sval src =
                      liftIO $ do
                          result <- specialEvaluate spvals tp src
                          return $ mapResultFailure showText result
              return $ MkSomeOf stype sval
        ]
