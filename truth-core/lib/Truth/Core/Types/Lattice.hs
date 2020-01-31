module Truth.Core.Types.Lattice where

import Truth.Core.Lens
import Truth.Core.Types.Pair
import Truth.Core.Types.ReadOnly

class JoinSemiLatticeReadOnlyEditLens update where
    joinEditLens :: EditLens (PairUpdate update update) (ReadOnlyUpdate update)

class MeetSemiLatticeReadOnlyEditLens update where
    meetEditLens :: EditLens (PairUpdate update update) (ReadOnlyUpdate update)
