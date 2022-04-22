module Import
    ( module I
    ) where

import Control.Applicative as I
import Control.Category as I
import Control.Concurrent.MVar as I
import Control.Monad as I hiding (fail)
import Control.Monad.Fail as I
import Control.Monad.Fix as I
import Data.Coerce as I
import Data.Constraint as I hiding (trans)
import Data.Foldable as I
import Data.Functor.Compose as I
import Data.Functor.Identity as I
import Data.Kind as I
import Data.Maybe as I
import Data.Monoid as I
import Data.String as I (IsString(..))
import Data.Traversable as I
import Data.Tuple as I
import Data.Void as I
import Data.Witness as I hiding (trans)
import Prelude as I hiding ((.), fail, id)
