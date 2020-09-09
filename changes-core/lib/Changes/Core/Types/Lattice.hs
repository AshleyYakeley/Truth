module Truth.Core.Types.Lattice where

import Truth.Core.Lens
import Truth.Core.Types.ReadOnly
import Truth.Core.Types.Tuple.Pair

class JoinSemiLatticeReadOnlyChangeLens update where
    joinChangeLens :: ChangeLens (PairUpdate update update) (ReadOnlyUpdate update)

class MeetSemiLatticeReadOnlyChangeLens update where
    meetChangeLens :: ChangeLens (PairUpdate update update) (ReadOnlyUpdate update)
