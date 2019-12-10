module Pinafore.Base.Lens where

import Truth.Core

class BaseEditLens edit baseupdate where
    baseEditLens :: EditLens baseupdate edit
