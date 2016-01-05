module Data.DICOM.Dictionary
       ( lookupElementByTag
       , loadElementDictionary
       , loadUIDDictionary
       , lookupUIDType
       , lookupUID
       ) where


import qualified Data.Bimap as DB
import qualified Data.ByteString as DBS
import Data.DICOM.Dictionary.Internal.InternalDictionary
import Data.DICOM.Model.Dictionary
import Data.DICOM.Model.RegisteredUID
import qualified Data.Map as DM
import Data.Word

-- | Lookup a dicom element based on the tag from the provided dicom dictionary
lookupElementByTag::DicomDictionary -> (Word16,Word16) -> Maybe DictElement
lookupElementByTag dd tag = DM.lookup tag dd

-- | Load the dicom element dictionary
loadElementDictionary::DicomDictionary
loadElementDictionary = DM.fromList dictElementList

loadUIDDictionary::UIDDictionary
loadUIDDictionary= DB.fromList dicomUIDTable

lookupUIDType::UIDDictionary -> DBS.ByteString -> DicomUID
lookupUIDType = (DB.!)

lookupUID::UIDDictionary -> DicomUID -> DBS.ByteString
lookupUID = (DB.!>)
