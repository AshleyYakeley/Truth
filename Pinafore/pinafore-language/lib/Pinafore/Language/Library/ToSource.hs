module Pinafore.Language.Library.ToSource where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Type

newtype ToSource = MkToSource {toSource :: PrecText}
    deriving newtype ToText

instance ShowPinafore ToSource where
    showPinafore = toSource

instance HasQGroundType '[] ToSource where
    qGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily ToSource)|]) "ToSource.Pinafore."

toSourceSubtype :: forall t. (HasQPartialGroundedType QPolyShim 'Negative t, ShowPinafore t) => LibraryStuff
toSourceSubtype =
    subtypeRelationBDS Verify "as Pinafore source text" qGroundedType qGroundedType
        $ functionToShim "conv"
        $ MkToSource
        . showPinafore @t
