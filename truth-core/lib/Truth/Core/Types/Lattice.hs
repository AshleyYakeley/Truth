module Truth.Core.Types.Lattice where

import Truth.Core.Edit
import Truth.Core.Types.Pair

class JoinSemiLatticeUpdateFunction update where
    joinUpdateFunction :: UpdateFunction (PairUpdate update update) update

class MeetSemiLatticeUpdateFunction update where
    meetUpdateFunction :: UpdateFunction (PairUpdate update update) update
