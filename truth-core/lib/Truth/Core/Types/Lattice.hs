module Truth.Core.Types.Lattice where

import Truth.Core.Edit
import Truth.Core.Types.Pair

class JoinSemiLatticeEdit edit where
    joinEditFunction :: PureEditFunction (PairEdit edit edit) edit

joinGeneralFunction :: JoinSemiLatticeEdit edit => GeneralFunction (PairEdit edit edit) edit
joinGeneralFunction = MkCloseState joinEditFunction

class MeetSemiLatticeEdit edit where
    meetEditFunction :: PureEditFunction (PairEdit edit edit) edit

meetGeneralFunction :: MeetSemiLatticeEdit edit => GeneralFunction (PairEdit edit edit) edit
meetGeneralFunction = MkCloseState meetEditFunction
