module Main
    ( main
    ) where

import qualified Options.Applicative as O
import Shapes
import System.FilePath hiding ((<.>))
import Truth.Core
import Truth.UI.GTK
import Truth.World.Charset
import Truth.World.File

textCodec :: ReasonCodec ByteString String
textCodec = utf8Codec . bijectionCodec packBijection

textLens :: EditLens ByteStringEdit (WholeEdit ((Result String) String))
textLens = (wholeEditLens $ injectionLens $ toInjection $ codecInjection textCodec) . convertEditLens

fileTextWindow :: Bool -> FilePath -> IO SomeUIWindow
fileTextWindow saveOpt path = do
    let
        bsObj :: Object ByteStringEdit
        bsObj = fileObject path
        wholeTextObj :: Object (WholeEdit ((Result String) String))
        wholeTextObj = cacheObject $ mapObject textLens bsObj
    if saveOpt
        then do
            let
                baseSub :: Subscriber (WholeEdit ((Result String) String)) ()
                baseSub = objectSubscriber wholeTextObj
                bufferSub :: Subscriber (OneWholeEdit (Result String) (StringEdit String)) ((), SaveActions)
                bufferSub = saveBufferSubscriber baseSub
                undoBufferSub ::
                       Subscriber (OneWholeEdit (Result String) (StringEdit String)) (((), SaveActions), UndoActions)
                undoBufferSub = undoQueueSubscriber bufferSub
            textSub <- makeSharedSubscriber undoBufferSub
            return $ MkSomeUIWindow $ MkUIWindow (takeFileName path) (uiOneWhole uiStringText) textSub
        else do
            let
                textObj :: Object (OneWholeEdit (Result String) (StringEdit String))
                textObj = convertObject wholeTextObj
            textSub <- makeObjectSubscriber textObj
            return $ MkSomeUIWindow $ MkUIWindow (takeFileName path) (uiOneWhole uiStringText) textSub

optParser :: O.Parser ([FilePath], Bool, Bool)
optParser = (,,) <$> (O.many $ O.strArgument mempty) <*> O.switch (O.short '2') <*> O.switch (O.long "save")

main :: IO ()
main =
    truthMain $ \args -> do
        (paths, double, save) <- O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) args
        wmss <-
            for paths $ \path -> do
                wm <- fileTextWindow save path
                return $
                    if double
                        then [wm, wm]
                        else [wm]
        return $ mconcat wmss
