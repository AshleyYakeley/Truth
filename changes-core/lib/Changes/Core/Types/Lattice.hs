module Changes.Core.Types.Lattice where

import Changes.Core.Lens
import Changes.Core.Types.ReadOnly
import Changes.Core.Types.Tuple.Pair

class JoinSemiLatticeReadOnlyChangeLens update where
    joinChangeLens :: ChangeLens (PairUpdate update update) (ReadOnlyUpdate update)

class MeetSemiLatticeReadOnlyChangeLens update where
    meetChangeLens :: ChangeLens (PairUpdate update update) (ReadOnlyUpdate update)
