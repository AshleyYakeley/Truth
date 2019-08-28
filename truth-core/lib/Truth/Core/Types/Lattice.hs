module Truth.Core.Types.Lattice where

import Truth.Core.Edit
import Truth.Core.Types.Pair

class JoinSemiLatticeEdit edit where
    joinUpdateFunction :: UpdateFunction (PairEdit edit edit) edit

class MeetSemiLatticeEdit edit where
    meetUpdateFunction :: UpdateFunction (PairEdit edit edit) edit
