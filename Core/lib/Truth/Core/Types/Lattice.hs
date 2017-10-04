module Truth.Core.Types.Lattice where
{
    import Truth.Core.Edit;
    import Truth.Core.Types.Pair;


    class JoinSemiLatticeEdit edit where
    {
        joinEditFunction :: PureEditFunction (PairEdit edit edit) edit;
    };

    class MeetSemiLatticeEdit edit where
    {
        meetEditFunction :: PureEditFunction (PairEdit edit edit) edit;
    };
}
