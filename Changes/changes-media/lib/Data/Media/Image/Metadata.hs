{-# OPTIONS -fno-warn-orphans #-}

module Data.Media.Image.Metadata
    ( ImageDataKey
    , Keys(..)
    , fromMetadatas
    , toMetadatas
    ) where

import Codec.Picture.Metadata
import Control.DeepSeq
import Shapes hiding (Format)

type ImageDataKey = Keys

instance WitnessConstraint Show ImageDataKey where
    witnessConstraint Gamma = Dict
    witnessConstraint ColorSpace = Dict
    witnessConstraint Format = Dict
    witnessConstraint DpiX = Dict
    witnessConstraint DpiY = Dict
    witnessConstraint Width = Dict
    witnessConstraint Height = Dict
    witnessConstraint Title = Dict
    witnessConstraint Description = Dict
    witnessConstraint Author = Dict
    witnessConstraint Copyright = Dict
    witnessConstraint Software = Dict
    witnessConstraint Comment = Dict
    witnessConstraint Disclaimer = Dict
    witnessConstraint Source = Dict
    witnessConstraint Warning = Dict
    witnessConstraint (Exif _) = Dict
    witnessConstraint (Unknown _) = Dict

instance WitnessConstraint NFData ImageDataKey where
    witnessConstraint Gamma = Dict
    witnessConstraint ColorSpace = Dict
    witnessConstraint Format = Dict
    witnessConstraint DpiX = Dict
    witnessConstraint DpiY = Dict
    witnessConstraint Width = Dict
    witnessConstraint Height = Dict
    witnessConstraint Title = Dict
    witnessConstraint Description = Dict
    witnessConstraint Author = Dict
    witnessConstraint Copyright = Dict
    witnessConstraint Software = Dict
    witnessConstraint Comment = Dict
    witnessConstraint Disclaimer = Dict
    witnessConstraint Source = Dict
    witnessConstraint Warning = Dict
    witnessConstraint (Exif _) = Dict
    witnessConstraint (Unknown _) = Dict

fromMetadatas :: Metadatas -> WitnessMapOf ImageDataKey
fromMetadatas = Codec.Picture.Metadata.foldMap $ \(k :=> v) -> witnessMapOfSingle k v

toMetadatas :: WitnessMapOf ImageDataKey -> Metadatas
toMetadatas wd =
    witnessMapOfFold wd $ \k v ->
        case (witnessConstraint @Type @Show k, witnessConstraint @Type @NFData k) of
            (Dict, Dict) -> Codec.Picture.Metadata.singleton k v
