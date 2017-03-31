module MIME (MIMEType(..)) where
{
    import Control.Monad;
    import Text.Read;
--    import Text.ParserCombinators.ReadPrec;


    data MIMEType = MkMIMEType String String [(String,String)];

    doit :: (Monad m) => m a -> m ();
    doit ma = ma >> return ();

    getList :: (MonadPlus m) => m a -> m [a];
    getList rc = mplus (do
    {
        c <- rc;
        s <- getList rc;
        return (c:s);
    }) (return []);

    doList :: (MonadPlus m) => m a -> m ();
    doList rc = mplus (do
    {
        rc;
        doList rc;
    }) (return ());

    readAllowed :: (Char -> Bool) -> ReadPrec Char;
    readAllowed ok = do
    {
        c <- get;
        if ok c
            then return c
            else mzero;
    };

    readChar :: Char -> ReadPrec ();
    readChar ec = do
    {
        c <- get;
        if ec == c
            then return ()
            else mzero;
    };

    readToken :: ReadPrec String;
    readToken = getList (readAllowed tokenChar) where
    {
        tokenChar :: Char -> Bool;
        tokenChar ' ' = False;
        tokenChar '(' = False;
        tokenChar ')' = False;
        tokenChar '<' = False;
        tokenChar '>' = False;
        tokenChar '@' = False;
        tokenChar ',' = False;
        tokenChar ';' = False;
        tokenChar ':' = False;
        tokenChar '\\' = False;
        tokenChar '"' = False;
        tokenChar '/' = False;
        tokenChar '[' = False;
        tokenChar ']' = False;
        tokenChar '?' = False;
        tokenChar '=' = False;
        tokenChar c = (fromEnum c) > 31;
    };

    readQPair :: ReadPrec Char;
    readQPair = do
    {
        readChar '\\';
        get;
    };

    readWhiteSpace :: ReadPrec ();
    readWhiteSpace = doList (mplus readComment (doit (readAllowed wsChar))) where
    {
        wsChar :: Char -> Bool;
        wsChar ' ' = True;
        wsChar '\t' = True;
        wsChar '\n' = True;
        wsChar _ = False;

        readComment :: ReadPrec ();
        readComment = do
        {
            readChar '(';
            doList (mplus readComment (mplus (doit readQPair) (doit (readAllowed cChar))));
            readChar ')';
        } where
        {
            cChar :: Char -> Bool;
            cChar '\\' = False;
            cChar '(' = False;
            cChar ')' = False;
            cChar '\n' = False;
            cChar _ = True;
        };
    };

    readQuotedString :: ReadPrec String;
    readQuotedString = do
    {
        readChar '"';
        s <- getList (mplus readQPair (readAllowed qChar));
        readChar '"';
        return s;
    } where
    {
        qChar :: Char -> Bool;
        qChar '"' = False;
        qChar '\\' = False;
        qChar '\n' = False;
        qChar _ = True;
    };

    readParam :: ReadPrec (String,String);
    readParam = do
    {
        readWhiteSpace;
        readChar ';';
        readWhiteSpace;
        attr <- readToken;
        readWhiteSpace;
        value <- mplus readToken readQuotedString;
        return (attr,value);
    };

    instance Read MIMEType where
    {
        readPrec = do
        {
            readWhiteSpace;
            tp <- readToken;
            readWhiteSpace;
            readChar '/';
            readWhiteSpace;
            sub <- readToken;
            params <- getList readParam;
            return (MkMIMEType tp sub params);
        };
    };

}
