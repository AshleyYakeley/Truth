module Pinafore.Language
    ( LibraryStuff
    , LibraryModule (..)
    , LoadModule
    , directoryLoadModule
    , textLoadModule
    , libraryLoadModule
    , QModule (..)
    , getModule
    , LibraryContext (..)
    , mkLibraryContext
    , pinaforeLibrary
    , QLocatedError
    , fromParseResult
    , InterpretResult
    , fromInterpretResult
    , runInterpretResult
    , Action
    , HasQType
    , parseTopExpression
    , parseToValue
    , parseToValueUnify
    , parseToValueSubsume
    , interact
    , initialPos
    , TopType (..)
    , Var
    , A
    , B
    , C
    , X
    , Y
    , Entity
    , runPinaforeScoped
    )
where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Error
import Pinafore.Language.Interpret
import Pinafore.Language.Interpreter
import Pinafore.Language.Library

interact :: (?library :: LibraryContext) => Handle -> Handle -> Bool -> View ()
interact inh outh echo = do
    liftIO $ hSetBuffering outh NoBuffering
    runInteract inh outh echo $ fromInterpretResult . runPinaforeScoped "<UNKNOWN>"
