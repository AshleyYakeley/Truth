{-# OPTIONS -fno-warn-orphans #-}

module Data.Media.Image.Metadata
    ( ImageDataKey
    , Keys (..)
    , Value (..)
    , fromMetadatas
    , toMetadatas
    )
where

import Codec.Picture.Metadata
import Control.DeepSeq
import Shapes hiding (Format)

type ImageDataKey = Keys

instance TestEquality ImageDataKey where
    testEquality Gamma Gamma = Just Refl
    testEquality ColorSpace ColorSpace = Just Refl
    testEquality DpiX DpiX = Just Refl
    testEquality DpiY DpiY = Just Refl
    testEquality Width Width = Just Refl
    testEquality Height Height = Just Refl
    testEquality Title Title = Just Refl
    testEquality Description Description = Just Refl
    testEquality Author Author = Just Refl
    testEquality Copyright Copyright = Just Refl
    testEquality Software Software = Just Refl
    testEquality Comment Comment = Just Refl
    testEquality Disclaimer Disclaimer = Just Refl
    testEquality Source Source = Just Refl
    testEquality Warning Warning = Just Refl
    testEquality Format Format = Just Refl
    testEquality (Unknown v1) (Unknown v2)
        | v1 == v2 = Just Refl
    testEquality (Exif t1) (Exif t2)
        | t1 == t2 = Just Refl
    testEquality _ _ = Nothing

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
