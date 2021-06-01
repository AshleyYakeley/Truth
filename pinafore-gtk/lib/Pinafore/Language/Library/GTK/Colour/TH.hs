module Pinafore.Language.Library.GTK.Colour.TH
    ( getColours
    ) where

import Data.Colour
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import Shapes
import Shapes.Numeric
import System.IO

getColourNames :: IO [String]
getColourNames = do
    colourNames <- System.IO.readFile "colour.names.txt"
    return $ words colourNames

sequenceTH :: [TH.TExp a] -> TH.Q (TH.TExp [a])
sequenceTH [] = [||[]||]
sequenceTH (a:aa) = [||$$(pure a) : $$(sequenceTH aa)||]

localNameTH :: String -> TH.Q (TH.TExp a)
localNameTH name = TH.unsafeTExpCoerce $ pure $ TH.VarE $ TH.mkName name

getColours :: TH.Q (TH.TExp [(String, Colour Double)])
getColours = do
    cnames <- liftIO getColourNames
    pairs <- for cnames $ \cname -> [||(cname, $$(localNameTH $ "Data.Colour.Names." <> cname))||]
    sequenceTH pairs
