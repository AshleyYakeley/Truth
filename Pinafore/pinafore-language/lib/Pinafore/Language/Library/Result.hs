{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Result
    ( resultLibSection
    ) where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Type
import Pinafore.Language.Var

-- Result
resultGroundType :: QGroundType '[ CoCCRVariance, CoCCRVariance] Result
resultGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily Result)|]) "Result"

instance HasQGroundType '[ CoCCRVariance, CoCCRVariance] Result where
    qGroundType = resultGroundType

resultLibSection :: LibraryStuff context
resultLibSection =
    headingBDS
        "Result"
        ""
        [ typeBDS
              "Result"
              ""
              (MkSomeGroundType resultGroundType)
              [ valPatBDS "Success" "Result representing success." (SuccessResult :: A -> Result BottomType A) $
                ImpureFunction $ \(v :: Result TopType A) ->
                    case v of
                        SuccessResult a -> Just (a, ())
                        _ -> Nothing
              , valPatBDS "Failure" "Result representing failure." (FailureResult :: E -> Result E BottomType) $
                ImpureFunction $ \(v :: Result E TopType) ->
                    case v of
                        FailureResult err -> Just (err, ())
                        _ -> Nothing
              ]
        , namespaceBDS "Result" $
          monadEntries @_ @(Result E) <>
          [ valBDS "mfix" "The fixed point of a Result." $ mfix @(Result E) @A
          , valBDS "fail" "Failure Result" (FailureResult :: E -> Result E BottomType)
          ]
        ]
