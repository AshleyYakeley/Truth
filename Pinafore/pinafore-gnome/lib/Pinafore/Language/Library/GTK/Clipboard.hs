module Pinafore.Language.Library.GTK.Clipboard
    ( clipboardStuff
    ) where

import Changes.Core
import Changes.World.GNOME.GTK
import Pinafore.API
import Pinafore.Language.Library.GTK.Context
import Shapes

clipBijection :: Bijection (Maybe Clip) (Know Literal)
clipBijection = let
    isoForwards :: Maybe Clip -> Know Literal
    isoForwards (Just (TextClip t)) = Known $ toLiteral t
    isoForwards _ = Unknown
    isoBackwards :: Know Literal -> Maybe Clip
    isoBackwards (Known l)
        | Just t <- fromLiteral l = Just $ TextClip t
    isoBackwards _ = Nothing
    in MkIsomorphism {..}

langClipboard :: LangContext -> LangWholeModel '( Literal, Literal)
langClipboard c =
    wModelToWholeModel $ eaMap (bijectionWholeChangeLens clipBijection) $ MkWModel $ ocClipboard $ lcOtherContext c

clipboardStuff :: LibraryStuff ()
clipboardStuff =
    headingBDS "Clipboard" "" [valBDS "clipboard" "The UI clipboard, for copying and pasting." langClipboard]
