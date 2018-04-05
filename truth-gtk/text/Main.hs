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

optParser :: O.Parser ([FilePath], Bool, Bool)
optParser = (,,) <$> (O.many $ O.strArgument mempty) <*> O.switch (O.short '2') <*> O.switch (O.long "save")

main :: IO ()
main =
    truthMain $ \args createWindow -> do
        (paths, double, saveOpt) <- O.handleParseResult $ O.execParserPure O.defaultPrefs (O.info optParser mempty) args
        for_ paths $ \path -> do
            let
                bsObj :: Object ByteStringEdit
                bsObj = fileObject path
                wholeTextObj :: Object (WholeEdit ((Result Text) Text))
                wholeTextObj = cacheObject $ mapObject textLens bsObj
            action <-
                if saveOpt
                    then do
                        let
                            baseSub :: Subscriber (WholeEdit ((Result Text) Text)) ()
                            baseSub = objectSubscriber $ pure wholeTextObj
                            bufferSub :: Subscriber (OneWholeEdit (Result Text) (StringEdit Text)) ((), SaveActions)
                            bufferSub = saveBufferSubscriber baseSub
                            undoBufferSub ::
                                   Subscriber (OneWholeEdit (Result Text) (StringEdit Text)) ( ((), SaveActions)
                                                                                             , UndoActions)
                            undoBufferSub = undoQueueSubscriber bufferSub
                        textSub <- makeSharedSubscriber undoBufferSub
                        return $
                            createWindow $
                            MkUserInterface textSub $
                            MkUIWindow (constEditFunction $ fromString $ takeFileName path) (uiOneWhole uiText)
                    else do
                        let
                            textObj :: Object (OneWholeEdit (Result Text) (StringEdit Text))
                            textObj = convertObject wholeTextObj
                        textSub <- makeObjectSubscriber textObj
                        return $
                            createWindow $
                            MkUserInterface textSub $
                            MkUIWindow (constEditFunction $ fromString $ takeFileName path) (uiOneWhole uiText)
            action
            if double
                then action
                else return ()
