{-# LANGUAGE ApplicativeDo #-}

{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Result
    ( resultLibSection
    )
where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule

resultFrom :: (E -> B) -> (A -> B) -> Result E A -> B
resultFrom eb _ (FailureResult e) = eb e
resultFrom _ ab (SuccessResult a) = ab a

resultLibSection :: LibraryStuff
resultLibSection =
    headingBDS
        "Result"
        ""
        [ typeBDS
            "Result"
            ""
            (MkSomeGroundType resultGroundType)
            [ addNameInRootBDS
                $ valPatBDS "Success" "Result representing success." (SuccessResult :: A -> Result BottomType A)
                $ ImpureFunction
                $ pure
                $ \(v :: Result TopType A) ->
                    case v of
                        SuccessResult a -> Just (a, ())
                        _ -> Nothing
            , addNameInRootBDS
                $ valPatBDS "Failure" "Result representing failure." (FailureResult :: E -> Result E BottomType)
                $ ImpureFunction
                $ pure
                $ \(v :: Result E TopType) ->
                    case v of
                        FailureResult err -> Just (err, ())
                        _ -> Nothing
            ]
        , hasSubtypeRelationBDS @(Result Entity Entity) @Entity Verify ""
            $ functionToShim "resultEntityConvert" resultEntityConvert
        , hasSubtypeRelationBDS @(Result Showable Showable) @Showable Verify "" $ functionToShim "show" textShowable
        , namespaceBDS "Result"
            $ monadEntries @(Result E)
            <> [ valBDS "mfix" "The fixed point of a Result." $ mfix @(Result E) @A
               , valBDS "fail" "Failure Result" (FailureResult :: E -> Result E BottomType)
               , valBDS "mapFailure" "" (mapResultFailure :: (B -> C) -> Result B A -> Result C A)
               , valBDS "from" "" resultFrom
               , valBDS "fromSum" "" (eitherToResult :: Either E A -> Result E A)
               , valBDS "toSum" "" (resultToEither :: Result E A -> Either E A)
               , valBDS "fromMaybe" "" (resultFromMaybe :: E -> Maybe A -> Result E A)
               , valBDS "toMaybe" "" (resultToMaybe :: Result E A -> _ A)
               , valBDS "toAction" "" (resultToM . mapResultFailure (unpack . unShowable) :: Result Showable A -> Action A)
               ]
        ]
