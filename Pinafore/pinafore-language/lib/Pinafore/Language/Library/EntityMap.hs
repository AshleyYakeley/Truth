module Pinafore.Language.Library.EntityMap
    ( mapLibSection
    )
where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Value

mapLibSection :: LibraryStuff
mapLibSection =
    headingBDS
        "EntityMap"
        ""
        [ typeBDS "EntityMap" "A hash map." (MkSomeGroundType entityMapGroundType) []
        , hasSubtypeRelationBDS @(EntityMap Entity) @Entity Verify ""
            $ functionToShim "mapEntityConvert" mapEntityConvert
        , namespaceBDS "EntityMap"
            $ monoidEntries @(EntityMap A)
            <> [ valBDS "lookup" "Look up element." $ entityMapLookup @A
               , valBDS "insert" "Insert into map." $ entityMapInsert @A
               , valBDS "delete" "Delete from map." $ entityMapDelete @A
               , valBDS "single" "A map with one element." $ entityMapSingle @A
               , valBDS "keys" "The keys of the map." $ entityMapKeys @TopType
               , valBDS "values" "The values of the map." $ entityMapValues @A
               , valBDS "fromList" "Construct from list." $ entityMapFromList @A
               , valBDS "toList" "Convert to list." $ entityMapToList @A
               ]
        ]
