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

textCodec :: ReasonCodec ByteString Text
textCodec = bijectionCodec packBijection . utf8Codec . bijectionCodec unpackBijection

textLens :: EditLens ByteStringEdit (WholeEdit ((Result Text) Text))
textLens = (wholeEditLens $ injectionLens $ toInjection $ codecInjection textCodec) . convertEditLens

fileTextWindow :: Bool -> FilePath -> IO SomeUIWindow
fileTextWindow saveOpt path = do
    let
        bsObj :: Object ByteStringEdit
        bsObj = fileObject path
        wholeTextObj :: Object (WholeEdit ((Result Text) Text))
        wholeTextObj = cacheObject $ mapObject textLens bsObj
    if saveOpt
        then do
            let
                baseSub :: Subscriber (WholeEdit ((Result Text) Text)) ()
                baseSub = objectSubscriber $ pure wholeTextObj
                bufferSub :: Subscriber (OneWholeEdit (Result Text) (StringEdit Text)) ((), SaveActions)
                bufferSub = saveBufferSubscriber baseSub
                undoBufferSub ::
                       Subscriber (OneWholeEdit (Result Text) (StringEdit Text)) (((), SaveActions), UndoActions)
                undoBufferSub = undoQueueSubscriber bufferSub
            textSub <- makeSharedSubscriber undoBufferSub
            return $
                MkSomeUIWindow $
                MkUserInterface textSub $
                MkUIWindow (constEditFunction $ fromString $ takeFileName path) (uiOneWhole uiText)
        else do
            let
                textObj :: Object (OneWholeEdit (Result Text) (StringEdit Text))
                textObj = convertObject wholeTextObj
            textSub <- makeObjectSubscriber textObj
            return $
                MkSomeUIWindow $
                MkUserInterface textSub $
                MkUIWindow (constEditFunction $ fromString $ takeFileName path) (uiOneWhole uiText)

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
