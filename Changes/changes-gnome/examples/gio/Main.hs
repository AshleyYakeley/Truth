module Main
    ( main
    ) where

import Changes.Core
import Changes.World.GNOME.GIO.File
import GI.Gio qualified as GI
import Shapes

main :: IO ()
main = do
    f <- GI.fileNewForPath "stack.yaml"
    ref <- giFileReference f
    mtbs <- runResource emptyResourceContext ref $ \aref -> readableToSubject $ refRead aref
    case mtbs of
        Nothing -> fail "no read"
        Just (_, bs) -> putStrLn $ unpack $ decodeUtf8 bs
    return ()
