module Truth.Core.UI.Request where

import Truth.Core.Import

witChooseFile :: IOWitness (IO (Maybe FilePath))
witChooseFile = $(iowitness [t|IO (Maybe FilePath)|])
