module Truth.Core.Types.Lattice where

import Truth.Core.Edit
import Truth.Core.Types.Pair

class JoinSemiLatticeEdit edit where
    joinEditFunction :: EditFunction (PairEdit edit edit) edit

class MeetSemiLatticeEdit edit where
    meetEditFunction :: EditFunction (PairEdit edit edit) edit
