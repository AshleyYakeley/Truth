module Browser where
{
    import Data.Changes;
    import File;
--    import Partial;
--    import Interpret;
--    import Object;
    import Data.Witness;
    import System.Glib.StoreValue;
--    import System.Gnome.VFS;
--    import System.Gnome.VFS.Types;
    import Graphics.UI.Gtk hiding (Object);
    import Distribution.PackageDescription;
    import Distribution.Simple.Utils;
--    import Distribution.PreProcess;
    import Text.Read;
--    import Data.Maybe;
--    import Data.Word;
    import Data.ByteString hiding (putStrLn);
    import Prelude hiding (read);
    
    type Selection context = [Any (Object context)];
    
    data Browser = forall w. (WidgetClass w) => MkBrowser
    {
        browserWidget :: w,
        browserSelection :: IO Selection,
        closeBrowser :: IO Bool
    };
    
    instance Show GenericValue where
    {
        show (GVstring (Just s)) = show s;
        show (GVstring _) = "<null string>";
        show _ = "<other type>"
    };

    uriObject :: URI -> Object (Maybe ByteString);
    uriObject uri = MkObject uri (MkReference getURI setURI) where
    {
        getURI :: IO (Maybe ByteString);
        getURI = do
        {
            catch (do
            {
                h <- openURI uri OpenRead;
                info <- getFileInfoFromHandle h [FileInfoDefault];
                Just fs <- return (fileInfoSize info);
                bs <- System.Gnome.VFS.read h fs;
                close h;
                return (Just bs);
            })
            (\_ -> return Nothing);
        };
        
        setURI :: Maybe ByteString -> IO ();
        setURI Nothing = unlinkFromURI uri;
        setURI (Just bs) = do
        {
            h <- openURI uri OpenWrite;
            write h bs;
            close h;
        };
    };

    type BrowserFactory a = Object a -> (Selection -> IO ()) -> IO Browser;
    
    pickBrowser :: ValueType a -> BrowserFactory a;
    pickBrowser t@(MaybeValueType ot) = maybeBrowser t (pickBrowser ot);
    pickBrowser (ListValueType CharValueType) = textBrowser;
    pickBrowser PackageDescriptionValueType = cabalBrowser;
    pickBrowser (SourceValueType _) = textBrowser;
    pickBrowser t = lastResortBrowser t;

    pickObjBrowser :: AnyObject -> (Selection -> IO ()) -> IO Browser;
    pickObjBrowser (MkAnyF ot obj) = pickBrowser ot obj;

    unMaybeObj :: Object (Maybe a) -> IO (Maybe (Object a));
    unMaybeObj (MkObject context ref) = do
    {
        ma <- getRef ref;
        case ma of
        {
            Just a -> return (Just (MkObject context (MkReference (return a) (\a' -> setRef ref (Just a')))));
            _ -> return Nothing;
        };
    };

    maybeBrowser :: ValueType (Maybe a) -> BrowserFactory a -> BrowserFactory (Maybe a);
    maybeBrowser t factory objm onSel = do
    {
        mobj <- unMaybeObj objm;
        case mobj of
        {
            Just obj -> factory obj onSel;
            _ -> lastResortBrowser t objm onSel;
        };
    };

    textBrowser :: BrowserFactory String;
    textBrowser (MkObject context ref) onSel = do
    {
        text <- getRef ref;
        buffer <- textBufferNew Nothing;
        textBufferSetText buffer text;
        view <- textViewNewWithBuffer buffer;
        onMoveCursor view (\_ _ _ -> do
        {
            sel <- getBufferSelection buffer;
            onSel sel;
        });
        return 
        (MkBrowser
            view
--            (return Nothing)
            (getBufferSelection buffer)
            (return True)
        );
    } where
    {
        getBufferSelection :: TextBuffer -> IO Selection;
        getBufferSelection buffer = do
        {
            m1 <- textBufferGetInsert buffer;
            m2 <- textBufferGetSelectionBound buffer;
            return [MkAnyF (ListValueType CharValueType) (MkObject context (MkReference (bufferGet m1 m2) (bufferSet m1 m2)))];
        } where
        {
            bufferGet :: TextMark -> TextMark -> IO String;
            bufferGet m1 m2 = do
            {
                i1 <- textBufferGetIterAtMark buffer m1;
                i2 <- textBufferGetIterAtMark buffer m2;
                textBufferGetSlice buffer i1 i2 True;
            };

            bufferSet :: TextMark -> TextMark -> String -> IO ();
            bufferSet m1 m2 text = do
            {
                i1 <- textBufferGetIterAtMark buffer m1;
                i2 <- textBufferGetIterAtMark buffer m2;
                textBufferDelete buffer i1 i2;
                inew <- textBufferGetIterAtMark buffer m1;
                textBufferInsert buffer inew text;
            };
        };
    };
    
    lastResortBrowser :: ValueType a -> BrowserFactory a;
    lastResortBrowser t _ _ = do
    {
        w <- labelNew (Just ("No Browser for "++(show t)));
        return (MkBrowser w 
--        (return Nothing)
         (return []) (return True));
    };
    
    cabalBrowser :: BrowserFactory PackageDescription;
    cabalBrowser (MkObject context ref) onSel = do
    {
        pd <- getRef ref;
        (store,view) <- makeTreeView context pd;
        return
        (MkBrowser
            view
--            (return Nothing)
            (getSelection context store view)
            (return True)
        );
    }
    where
    {
        makeItem :: TreeStore -> Maybe TreeIter -> String -> Maybe String -> IO TreeIter;
        makeItem store mt s0 ms1 = do
        {
            iter <- treeStoreAppend store mt;
            treeStoreSetValue store iter 0 (GVstring (Just s0));
            treeStoreSetValue store iter 1 (GVstring ms1);
            return iter;
        };

        suffixes :: [String];
        suffixes = (fmap fst knownSuffixHandlers) ++ ["hs","lhs"];

        makeModuleItem :: TreeStore -> TreeIter -> BuildInfo -> String -> IO TreeIter;
        makeModuleItem store ti info fname = do
        {
            pathl <- moduleToFilePath (hsSourceDirs info) fname suffixes;
            mpath <- case pathl of
            {
                path:_ -> return (Just path);
                _ -> return Nothing;
            };
            makeItem store (Just ti) fname mpath;
        };
        
        addBuildInfo :: TreeStore -> TreeIter -> BuildInfo -> IO ();
        addBuildInfo store ti info = do
        {
            itHidden <- makeItem store (Just ti) "Hidden" Nothing;
            mapM_ (makeModuleItem store itHidden info) (otherModules info);
        };
        
        makeLibraryFolder :: TreeStore -> (Maybe TreeIter) -> Library -> IO ();
        makeLibraryFolder store mti lib = do
        {
            itLib <- makeItem store mti "Library" Nothing;
            mapM_ (makeModuleItem store itLib (libBuildInfo lib)) (exposedModules lib);
            addBuildInfo store itLib (libBuildInfo lib);
        };
        
        makeExecutableFolder :: TreeStore -> (Maybe TreeIter) -> Executable -> IO ();
        makeExecutableFolder store mti exe = do
        {
            itExe <- makeItem store mti (exeName exe) Nothing;
            makeItem store (Just itExe) (modulePath exe) (Just (modulePath exe));
            addBuildInfo store itExe (buildInfo exe);
        };

        getSelection :: URI -> TreeStore -> TreeView -> IO Selection;
        getSelection baseURI store tv = do
        {
            sel <- treeViewGetSelection tv;
            (Just ti) <- treeSelectionGetSelected sel;
            (GVstring (Just _)) <- treeModelGetValue store ti 0;
            v1 <- treeModelGetValue store ti 1;
            case v1 of
            {
                GVstring (Just fp) -> case uriAppendFileName baseURI fp of
                {
                    Just uri -> do
                    {
                        mime <- getMIMETypeCommon uri;
                        let 
                        {
                            interpreter = mimeInterpreter (Text.Read.read mime);
                            uriobj = codecMapObj byteStringCodec (uriObject uri);
                            selectedObj = interpretMaybe interpreter uriobj;
                        };
                        return [selectedObj];
                    };
                    _ -> return [];
                };
                _ -> return [];
            };
        };

        makeTreeView :: URI -> PackageDescription -> IO (TreeStore,TreeView);
        makeTreeView baseURI descr = do
        {
            store <- treeStoreNew [TMstring,TMstring];
            case library descr of
            {
                Just lib -> makeLibraryFolder store Nothing lib;
                _ -> return ();
            };
            mapM_ (makeExecutableFolder store Nothing) (executables descr);
            _ <- makeItem store Nothing "Properties" Nothing;
            tv <- treeViewNewWithModel store;
            rText <- cellRendererTextNew;
            c0 <- treeViewColumnNewWithAttributes "Name" rText [("text",0)];
             treeViewAppendColumn tv c0;
            treeViewSetHeadersVisible tv True;
            onCursorChanged tv (do
            {
                sel <- getSelection baseURI store tv;
                onSel sel;
            });
            return (store,tv);
        };
    };
}
