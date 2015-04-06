module DICOM.Dictionary (Dictionary(..),DictionaryElement(..)) where

type VR = String
type VM = String
type ElementName = String
type ElementKeyWord = String
type Version = String

data DictionaryElement = DictionaryElement { dicomGroup::String
                                           , dicomElement::String  
                                           , elementName::ElementName
                                           , elementKeyWord::ElementKeyWord
                                           , vr::VR
                                           , vm::VM
                                           , elementVersion::Version
                                           } deriving (Show,Eq,Read)

data Dictionary = Dictionary { elements::[DictionaryElement]} deriving (Show,Eq,Read)
