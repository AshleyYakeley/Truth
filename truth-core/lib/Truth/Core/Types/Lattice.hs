module Truth.Core.Types.Lattice where

import Truth.Core.Lens
import Truth.Core.Types.ReadOnly
import Truth.Core.Types.Tuple.Pair

class JoinSemiLatticeReadOnlyEditLens update where
    joinEditLens :: EditLens (PairUpdate update update) (ReadOnlyUpdate update)

class MeetSemiLatticeReadOnlyEditLens update where
    meetEditLens :: EditLens (PairUpdate update update) (ReadOnlyUpdate update)
