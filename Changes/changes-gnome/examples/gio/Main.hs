module Main
    ( main
    ) where

import qualified GI.Gio as GI
import Shapes

main :: IO ()
main = do
    f <- GI.fileParseName "stack.yaml"
    mbn <- GI.fileGetBasename f
    case mbn of
        Nothing -> fail "no basename"
        Just _ -> return ()
