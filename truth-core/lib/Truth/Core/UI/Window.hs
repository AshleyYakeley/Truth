module Truth.Core.UI.Window where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Map
import Truth.Core.UI.Specifier.MenuBar
import Truth.Core.UI.Specifier.Specifier

data WindowSpec edit = forall sel. MkWindowSpec
    { wsCloseBoxAction :: IO ()
    , wsTitle :: EditFunction edit (WholeEdit Text)
    , wsMenuBar :: Maybe (Aspect sel -> EditFunction edit (WholeEdit (MenuBar edit)))
    , wsContent :: UISpec sel edit
    }

mapWindowSpec :: EditLens edita editb -> WindowSpec editb -> WindowSpec edita
mapWindowSpec lens (MkWindowSpec cba title mmbar content) = let
    ef = editLensFunction lens
    in MkWindowSpec
           cba
           (title . ef)
           ((fmap $ fmap $ \efmar -> funcEditFunction (fmap $ mapMenuEntry ef) . efmar . ef) mmbar)
           (mapUISpec lens content)

data UIWindow = MkUIWindow
    { uiWindowHide :: IO ()
    , uiWindowShow :: IO ()
    }

nullUIWindow :: UIWindow
nullUIWindow = let
    uiWindowHide = return ()
    uiWindowShow = return ()
    in MkUIWindow {..}
