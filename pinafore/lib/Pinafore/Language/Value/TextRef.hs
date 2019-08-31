module Pinafore.Language.Value.TextRef where

import Pinafore.Base
import Shapes
import Truth.Core

type PinaforeTextRef baseupdate = PinaforeLensValue baseupdate (StringEdit Text)
