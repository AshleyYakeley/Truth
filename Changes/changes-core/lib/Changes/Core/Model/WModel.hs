module Changes.Core.Model.WModel where

import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Model.EditContext
import Changes.Core.Model.Model
import Changes.Core.Model.Reference
import Changes.Core.Model.Tuple
import Changes.Core.Model.WReference
import Changes.Core.Read
import Changes.Core.Resource
import Changes.Core.Types

newtype WModel update = MkWModel
    { unWModel :: Model update
    }

instance EditApplicative WModel where
    eaPure subj = MkWModel $ constantModel subj
    eaMap lens (MkWModel sv) = MkWModel $ mapModel lens sv
    eaPair (MkWModel sva) (MkWModel svb) = MkWModel $ pairModels sva svb

instance FloatingEditApplicative WModel where
    eaFloatMap rc flens (MkWModel sub) = fmap MkWModel $ floatMapModel rc flens sub

wModelGet :: ResourceContext -> WModel update -> ReadM (UpdateReader update) t -> IO t
wModelGet rc (MkWModel sub) readm = runResource rc sub $ \asub -> unReadM readm $ aModelRead asub

wModelPush :: ResourceContext -> WModel update -> NonEmpty (UpdateEdit update) -> IO Bool
wModelPush rc (MkWModel sub) edits = runResource rc sub $ \asub -> pushEdit noEditSource $ aModelEdit asub edits

wModelReference :: WModel update -> WReference update
wModelReference (MkWModel model) = MkWReference $ modelReference model
