module Truth.Core
(
    module Truth.Core.Sequence,
    module Truth.Core.Read,
    module Truth.Core.Edit,
    module Truth.Core.Types,
    module Truth.Core.Object,
    module Truth.Core,
) where
{
    import Truth.Core.Import;
    import Truth.Core.Sequence;
    import Truth.Core.Read;
    import Truth.Core.Edit;
    import Truth.Core.Types;
    import Truth.Core.Object;

    generalTypeKnowledge :: TypeKnowledge;
    generalTypeKnowledge = mconcat
    [
        baseTypeKnowledge,
        typeInfoKnowledge $ typeInfo @ReadableConstraint,
        typeInfoKnowledge $ typeInfo @OneEdit
    ];
}
