module Pinafore.Base.Lens where

import Truth.Core

class BaseChangeLens edit baseupdate where
    baseChangeLens :: ChangeLens baseupdate edit
