module Pinafore.Language.Value.TextRef where

import Pinafore.Base
import Shapes
import Truth.Core

type PinaforeTextRef baseedit = PinaforeLensValue baseedit (StringEdit Text)
