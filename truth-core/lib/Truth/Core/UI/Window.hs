module Truth.Core.UI.Window where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Types
import Truth.Core.UI.Specifier.Map
import Truth.Core.UI.Specifier.MenuBar
import Truth.Core.UI.Specifier.Specifier

data WindowSpec edit = forall sel. MkWindowSpec
    { wsTitle :: EditFunction edit (WholeEdit Text)
    , wsMenuBar :: Maybe (EditFunction edit (WholeEdit (MenuBar edit)))
    , wsContent :: UISpec sel edit
    }

mapWindowSpec :: EditLens edita editb -> WindowSpec editb -> WindowSpec edita
mapWindowSpec lens (MkWindowSpec title mmbar content) = let
    ef = editLensFunction lens
    in MkWindowSpec
           (title . ef)
           (fmap (\efmar -> funcEditFunction (fmap $ mapMenuEntry ef) . efmar . ef) mmbar)
           (mapUISpec lens content)

data UIWindow = MkUIWindow
    { uiWindowClose :: IO ()
    }

nullUIWindow :: UIWindow
nullUIWindow = MkUIWindow $ return ()
