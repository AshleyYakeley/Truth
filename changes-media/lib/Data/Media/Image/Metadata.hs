{-# OPTIONS -fno-warn-orphans #-}

module Data.Media.Image.Metadata where

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

fromMetadatas :: Metadatas -> WitnessDict ImageDataKey
fromMetadatas = Codec.Picture.Metadata.foldMap $ \(k :=> v) -> witnessDictSingle k v

toMetadatas :: WitnessDict ImageDataKey -> Metadatas
toMetadatas wd =
    witnessDictFold wd $ \k v ->
        case (witnessConstraint @Type @Show k, witnessConstraint @Type @NFData k) of
            (Dict, Dict) -> Codec.Picture.Metadata.singleton k v
