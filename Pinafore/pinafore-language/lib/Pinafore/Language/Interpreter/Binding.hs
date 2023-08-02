module Pinafore.Language.Interpreter.Binding
    ( QSpecialVals(..)
    , QSignature(..)
    , QRecordConstructor(..)
    , QSpecialForm(..)
    , QInterpreterBinding(..)
    , QBindingInfo(..)
    ) where

import Data.Shim
import Language.Expression.Common
import Pinafore.Base
import Pinafore.Language.DefDoc
import Pinafore.Language.Name
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type.Ground
import Pinafore.Language.Type.Identified
import Pinafore.Language.Type.Subtype ()
import Pinafore.Text
import Shapes

newtype QSpecialVals = MkQSpecialVals
    { specialEvaluate :: forall t. QType 'Positive t -> Text -> Action (Either Text t)
        -- ^ in Action because this can do things like import files
    }

data QSignature (polarity :: Polarity) (t :: Type) =
    ValueSignature TypeID
                   Name
                   (QType polarity t)
                   (Maybe (QOpenExpression t))

instance (HasInterpreter, Is PolarityType polarity) => HasVarMapping (QSignature polarity) where
    getVarMapping (ValueSignature _ _ t _) = getVarMapping t

data QRecordConstructor =
    forall (t :: Type) (tt :: [Type]). MkQRecordConstructor (ListVType (QSignature 'Positive) tt)
                                                            (QGroundedShimWit 'Positive t)
                                                            (QGroundedShimWit 'Negative t)
                                                            (Codec t (ListVProduct tt))

type QSpecialForm :: Type
data QSpecialForm =
    forall lt. MkQSpecialForm (ListType QAnnotation lt)
                              (ListProduct lt -> Interpreter QValue)

data QInterpreterBinding
    = ValueBinding QExpression
                   (Maybe QPatternConstructor)
    | TypeBinding QSomeGroundType
    | RecordConstructorBinding QRecordConstructor
    | SpecialFormBinding QSpecialForm

instance HasInterpreter => Show QInterpreterBinding where
    show (ValueBinding e Nothing) = "val: " <> show e
    show (ValueBinding e (Just _)) = "val+pat: " <> show e
    show (TypeBinding t) = "type: " <> unpack (toText $ exprShow t)
    show (RecordConstructorBinding _) = "recordcons"
    show (SpecialFormBinding _) = "special"

data QBindingInfo = MkQBindingInfo
    { biOriginalName :: FullName
    , biDocumentation :: DefDoc
    , biValue :: QInterpreterBinding
    }
