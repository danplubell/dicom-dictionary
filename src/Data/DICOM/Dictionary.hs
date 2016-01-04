module Data.DICOM.Dictionary
       ( lookupElementByTag
       , loadElementDictionary
       ) where


import Data.DICOM.Dictionary.Internal.InternalDictionary
import Data.DICOM.Model.Dictionary
import qualified Data.Map as DM
import Data.Word

-- | Lookup a dicom element based on the tag from the provided dicom dictionary
lookupElementByTag::DicomDictionary -> (Word16,Word16) -> Maybe DictElement
lookupElementByTag dd tag = DM.lookup tag dd

-- | Load the dicom element dictionary
loadElementDictionary::DicomDictionary
loadElementDictionary = DM.fromList dictElementList
