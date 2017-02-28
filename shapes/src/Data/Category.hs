module Data.Category  where
{
    import Data.Kind;
    import Control.Category;

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
}
