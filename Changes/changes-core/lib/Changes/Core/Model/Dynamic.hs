module Changes.Core.Model.Dynamic where

--import Changes.Core.Edit
import Changes.Core.Import
import Changes.Core.Model.Model
import Changes.Core.Model.WModel
--import Changes.Core.Lens
--import Changes.Core.Model.EditContext
--import Changes.Core.Model.Premodel
--import Changes.Core.Model.Reference
--import Changes.Core.Read
import Changes.Core.Resource
import Changes.Core.Types

dynamicModel :: forall update. Model (ROWUpdate (Model update)) -> Model update
dynamicModel (MkResource runner am) = MkResource runner $ error "NYI" am

dynamicWModel :: forall update. WModel (ROWUpdate (WModel update)) -> WModel update
dynamicWModel wmodel = MkWModel $ dynamicModel $ unWModel $ eaMapReadOnlyWhole unWModel wmodel
