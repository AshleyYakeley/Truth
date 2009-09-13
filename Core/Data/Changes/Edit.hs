module Data.Changes.Edit where
{
    import Data.ConstFunction;
    import Data.Witness;
    import Data.Nothing;
    import Control.Category;
    import Prelude hiding (id,(.));

    class Edit edit where
    {
        type Subject edit;
        applyEdit :: edit -> ConstFunction (Subject edit) (Subject edit);
        invertEdit :: edit -> Subject edit -> Maybe edit;    -- "Nothing" means no change
        
        --type EditEvidence edit = ();
        type EditEvidence edit;
        editEvidence :: Type edit -> EditEvidence edit;
        --editEvidence _ = ();
    };

    data EditInst edit where
    {
        MkEditInst :: forall edit. (Edit edit) => EditInst edit;
    };

    applyAndInvertEdit :: (Edit edit) => edit -> (ConstFunction (Subject edit) (Subject edit),(Subject edit) -> Maybe edit);
    applyAndInvertEdit edit = (applyEdit edit,invertEdit edit);
    
    applyEdits :: (Edit edit) => [edit] -> ConstFunction (Subject edit) (Subject edit);
    applyEdits [] = id;
    applyEdits (e:es) = (applyEdits es) . (applyEdit e);

    commutableEdits :: (Edit edit, Eq (Subject edit)) => edit -> edit -> Subject edit -> Maybe (Subject edit);
    commutableEdits e1 e2 a = let
    {
        cf1 = applyEdit e1;
        cf2 = applyEdit e2;
        cf12 = cf1 . cf2;
        cf21 = cf2 . cf1;
        a12 = applyConstFunction cf12 a;
        a21 = applyConstFunction cf21 a;
    } in if a12 == a21 then Just a12 else Nothing;
    
    class (Edit edit) => FullEdit edit where
    {
        replaceEdit :: Subject edit -> edit;
    };

    data FullEditInst edit where
    {
        MkFullEditInst :: forall edit. (FullEdit edit) => FullEditInst edit;
    };
     
    newtype NoEdit a = MkNoEdit Nothing;

    instance Edit (NoEdit a) where
    {
        type Subject (NoEdit a) = a;
        applyEdit (MkNoEdit n) = never n;
        invertEdit (MkNoEdit n) = never n;
        
        type EditEvidence (NoEdit a) = ();
        editEvidence _ = ();
    };
   
    instance (Edit ea,Edit eb,Subject ea ~ Subject eb) => Edit (Either ea eb) where
    {
        type Subject (Either ea eb) = Subject ea;
        
        applyEdit (Left edit) = applyEdit edit;
        applyEdit (Right edit) = applyEdit edit;
        
        invertEdit (Left edit) s = fmap Left (invertEdit edit s);
        invertEdit (Right edit) s = fmap Right (invertEdit edit s);
        
        type EditEvidence (Either ea eb) = ();
        editEvidence _ = ();
    };
   
    instance (FullEdit ea,Edit eb,Subject ea ~ Subject eb) => FullEdit (Either ea eb) where
    {
        replaceEdit s = Left (replaceEdit s);
    };
}
