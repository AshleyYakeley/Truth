module Pinafore.Base.Lens where

import Truth.Core

class BaseEditLens edit baseedit where
    baseEditLens :: EditLens baseedit edit
