module Pinafore.Language.Library.Map
    ( mapLibSection
    ) where

import Import
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Optics ()
import Pinafore.Language.Type
import Pinafore.Language.Value
import Pinafore.Language.Var

mapLibSection :: LibraryStuff context
mapLibSection =
    headingBDS
        "Map"
        ""
        [ typeBDS "Map" "A hash map." (MkSomeGroundType mapGroundType) []
        , hasSubtypeRelationBDS @(LangMap Entity) @Entity Verify "" $ functionToShim "mapEntityConvert" mapEntityConvert
        , namespaceBDS "Map" $
          monoidEntries @_ @(LangMap A) <>
          [ valBDS "lookup" "Look up element." $ langMapLookup @A
          , valBDS "insert" "Insert into map." $ langMapInsert @A
          , valBDS "delete" "Delete from map." $ langMapDelete @A
          , valBDS "single" "A map with one element." $ langMapSingle @A
          , valBDS "keys" "The keys of the map." $ langMapKeys @TopType
          , valBDS "values" "The values of the map." $ langMapValues @A
          , valBDS "fromList" "Construct from list." $ langMapFromList @A
          , valBDS "toList" "Convert to list." $ langMapToList @A
          ]
        ]
