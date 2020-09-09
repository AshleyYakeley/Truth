module Pinafore.Base.Lens where

import Changes.Core

class BaseChangeLens edit baseupdate where
    baseChangeLens :: ChangeLens baseupdate edit
