module Data.Category  where
{
    import Shapes.Import;


    class Category cat => TerminalCategory (cat :: k -> k -> *) where
    {
        type Terminal cat :: k;

        terminal :: forall (a :: k). cat a (Terminal cat);
    };

    instance TerminalCategory (->) where
    {
        type Terminal (->) = ();
        terminal _ = ();
    };

    instance TerminalCategory (:-) where
    {
        type Terminal (:-) = (() :: Constraint);
        terminal = Sub Dict;
    };
}
