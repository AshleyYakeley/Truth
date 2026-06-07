module Pinafore.Library.GTK.Clipboard
    ( clipboardStuff
    )
where

import Changes.Core
import Pinafore.API
import Shapes

import Pinafore.Library.GTK.Context

clipBijection :: Bijection [Media] (Know Literal)
clipBijection = let
    isoForwards :: [Media] -> Know Literal
    isoForwards (MkMedia (MkMediaType "text" "plain" _) bs : _) =
        maybe Unknown (Known . toLiteral) $ resultToMaybe $ decode utf8Codec bs
    isoForwards _ = Unknown
    isoBackwards :: Know Literal -> [Media]
    isoBackwards (Known l)
        | Just t <- fromLiteral l = [MkMedia (MkMediaType "text" "plain" []) $ encode utf8Codec t]
    isoBackwards _ = []
    in MkIsomorphism{..}

langClipboard :: LangContext -> LangWholeModel '(Literal, Literal)
langClipboard c =
    wModelToWholeModel $ eaMap (bijectionWholeChangeLens clipBijection) $ MkWModel $ ocClipboard $ lcOtherContext c

clipboardStuff :: LibraryStuff
clipboardStuff =
    headingBDS "Clipboard" "" [valBDS "clipboard" "The UI clipboard, for copying and pasting." langClipboard]
