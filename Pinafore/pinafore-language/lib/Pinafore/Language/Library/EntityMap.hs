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
    mconcat
        [ headingBDS
            "EntityMap"
            ""
            [ typeBDS_ @_ @EntityMap "EntityMap" "A hash map." []
            , hasSubtypeRelationBDS @(EntityMap Entity) @Entity Verify ""
                $ functionToShim "mapEntityConvert" mapEntityConvert
            , namespaceBDS "EntityMap"
                $ monoidEntries @(EntityMap A)
                <> [ valBDS "lookup" "Look up element." $ entityMapLookup @A
                   , valBDS "member" "Whether this key is in the map." $ entityMapMember @A
                   , valBDS "contains" "Whether this key is in the map." $ entityMapContains @A
                   , valBDS "insert" "Insert into map." $ entityMapInsert @A
                   , valBDS "delete" "Delete from map." $ entityMapDelete @A
                   , valBDS "single" "A map with one element." $ entityMapSingle @A
                   , valBDS "keys" "The keys of the map." $ entityMapKeys @TopType
                   , valBDS "values" "The values of the map." $ entityMapValues @A
                   , valBDS "fromList" "Construct from list." $ entityMapFromList @A
                   , valBDS "toList" "Convert to list." $ entityMapToList @A
                   ]
            ]
        , headingBDS
            "EntitySet"
            ""
            [ typeBDS_ @_ @EntitySet
                "EntitySet"
                "A hash set, same as `EntityMap Unit`."
                [ hasBiNeutralSubtypeRelationBDS @EntitySet @(EntityMap ())
                ]
            , namespaceBDS "EntitySet"
                $ monoidEntries @EntitySet
                <> [ valBDS "member" "Whether this key is in the set, same as `member.EntityMap`." entitySetMember
                   , valBDS "contains" "Whether this key is in the set, same as `contains.EntityMap`." entitySetContains
                   , valBDS "insert" "Insert into set." entitySetInsert
                   , valBDS "delete" "Delete from set, same as `delete.EntityMap`." entitySetDelete
                   , valBDS "single" "A set with one element." entitySetSingle
                   , valBDS "fromList" "Construct from list." entitySetFromList
                   , valBDS "toList" "Convert to list, same as `keys.EntityMap`." entitySetToList
                   ]
            ]
        ]
