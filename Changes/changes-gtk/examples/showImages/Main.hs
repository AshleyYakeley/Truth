module Main
    ( main
    ) where

import Changes.Core
import Changes.UI.GTK
import Data.Media.Image
import Shapes
import System.Directory
import System.FilePath

imagedir :: FilePath
imagedir = "changes-gtk/examples/showImages/images"

resultTextToM :: MonadFail m => Result Text a -> m a
resultTextToM = resultToM . mapResultFailure unpack

main :: IO ()
main = do
    filenames <- listDirectory imagedir
    --(paths, double, selTest, saveOpt) <- O.execParser (O.info optParser mempty)
    runLifeCycleT $ do
        gtkContext <- runGTK
        runNewView $
            runGView gtkContext $ do
            -- fileReference :: FilePath -> Reference ByteStringEdit
                imageRef <- gvLiftIONoUI $ makeMemoryReference (blankImage @PixelRGB8 (100, 100) black) $ \_ -> True
                model <- gvLiftLifeCycleNoUI $ makeReflectingModel imageRef
                rec
                    (_, closer) <-
                        gvRunLocked $
                        gvGetState $
                        createWindow $ let
                            wsPosition = WindowPositionCenter
                            wsSize = (600, 600)
                            wsCloseBoxAction :: GView 'Locked ()
                            wsCloseBoxAction = gvCloseState closer
                            wsTitle :: Model (ROWUpdate Text)
                            wsTitle = constantModel "Images"
                            setFileRef :: FilePath -> MenuEntry
                            setFileRef filename =
                                ActionMenuEntry (pack filename) Nothing $
                                constantModel $
                                Just $ do
                                    bs <- liftIO $ readFile $ imagedir </> filename
                                    image <-
                                        resultTextToM $ do
                                            (_, jpegImage) <- decode (jpegFormat 50) bs
                                            decode jpegImageTrue8 jpegImage
                                    liftIO $
                                        runResource emptyResourceContext model $ \asub -> do
                                            _ <-
                                                pushEdit noEditSource $ aModelEdit asub $ pure $ MkWholeReaderEdit image
                                            return ()
                            wsMenuBar :: Maybe (Model (ROWUpdate MenuBar))
                            wsMenuBar = Just $ constantModel $ pure $ SubMenuEntry "Image" $ fmap setFileRef filenames
                            wsContent :: GView 'Locked Widget
                            wsContent = createImage $ mapModel toReadOnlyChangeLens model
                            in MkWindowSpec {..}
                return ()
