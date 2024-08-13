module Changes.Core.Model.WReference where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Model.EditContext
import Changes.Core.Model.Reference
import Changes.Core.Model.Tuple
import Changes.Core.Read
import Changes.Core.Resource
import Changes.Core.Types

newtype WReference update = MkWReference
    { unWReference :: Reference (UpdateEdit update)
    }

instance EditApplicative WReference where
    eaPure subj = MkWReference $ constantReference subj
    eaMap lens (MkWReference sv) = MkWReference $ mapReference lens sv
    eaPair (MkWReference sva) (MkWReference svb) = MkWReference $ pairReferences sva svb

instance FloatingEditApplicative WReference where
    eaFloatMap rc flens (MkWReference ref) = liftIO $ fmap MkWReference $ floatMapReference rc flens ref

wReferenceGet :: ResourceContext -> WReference update -> ReadM (UpdateReader update) t -> IO t
wReferenceGet rc (MkWReference ref) readm = runResource rc ref $ \asub -> unReadM readm $ refRead asub

wReferencePush :: ResourceContext -> WReference update -> NonEmpty (UpdateEdit update) -> IO Bool
wReferencePush rc (MkWReference ref) edits = runResource rc ref $ \asub -> pushEdit noEditSource $ refEdit asub edits
