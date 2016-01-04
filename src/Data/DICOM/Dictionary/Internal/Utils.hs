{-# LANGUAGE OverloadedStrings #-}
module Data.DICOM.Dictionary.Internal.Utils where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.DICOM.Model.Dictionary as DM
import qualified Data.Map as DM
import Data.Maybe
import Data.Word
import Numeric
import Paths_dicom_dictionary
import Prelude hiding (takeWhile)

genInternalDictionary::IO ()
genInternalDictionary = do
  fp <- getDataFileName "data/dicom.dic"
  d <- parseDictionary fp
  printTemplate  d

printTemplate:: [DM.DictElement] -> IO ()
printTemplate l  = do
  putStrLn "{-# LANGUAGE OverloadedStrings#-}"
  putStrLn "module Data.DICOM.InternalDictionary where"
  putStrLn "import Data.DICOM.Model.Dictionary"
  putStrLn "dictElementList = ["
  mapM_ printLine l
  putStrLn "    ]"
  where printLine  e  =
           case e of
              DM.Comment -> print e
              DM.Skip    -> print e
              _           ->
                       putStrLn $ "    ,(("
                       ++ "0x" ++ showHex (DM.groupNbr e) "" ++ ","
                       ++ "0x" ++ showHex (DM.elementNbr e) "" ++ "),"
                       ++ "DictElement " ++ "0x" ++ showHex (DM.groupNbr e) "" ++ " 0x"
                       ++ showHex (DM.elementNbr e) ""
                       ++ " " ++ show (DM.vr e)
                       ++ " " ++ show (DM.vm e)
                       ++ " " ++ show (DM.name e)
                       ++ " " ++ show (DM.version e)
                       ++ ")"


loadDictionaryFile:: IO DM.DicomDictionary
loadDictionaryFile = do
  fp <- getDataFileName "data/dicom.dic"
  mkDictionary <$> parseDictionary fp

mkDictionary::[DM.DictElement] -> DM.DicomDictionary
mkDictionary =
  DM.fromList.map mkMapEntry
  where mkMapEntry de = ((DM.groupNbr de, DM.elementNbr de), de)

parseDictionary::FilePath -> IO [DM.DictElement]
parseDictionary fp = do
    contents <- BS.readFile fp
    let parsed =  parseOnly dictionaryParser contents
    case parsed of
      Left s  -> error $ "A parse error occured: " ++ s
      Right l -> return $ filter skipNonDictElement  $ concat l
    where skipNonDictElement e = e /= DM.Comment && e /= DM.Skip

dictionaryParser:: Parser [[DM.DictElement]]
dictionaryParser = many' parseDataElement

parseDataElement::Parser [DM.DictElement]
parseDataElement = do
    frstChar <- anyChar
    case frstChar of
      '#' ->  do
             _ <- takeTill (=='\n')
             _ <- char '\n'
             return [DM.Comment]
      _   ->  do
          grpNbr <- takeTill (==',')
          _ <- char ','
          elemNbr <- takeTill (==')')
          _ <- char ')'
          _ <- char '\t'
          vrv <- takeWhile (/='\t')
          _ <- char '\t'
          n <- takeWhile (/= '\t')
          skipWhile isSpace
          vmv <- takeWhile (/= '\t')
          _ <- char '\t'
          ver <- takeWhile (/= '\n')
          _ <- char '\n'
          return $ mkElements grpNbr elemNbr vrv n vmv ver

mkElements::BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString-> BS.ByteString-> [DM.DictElement]
mkElements grp ele vrv nam vmv ver =
  case grp of
    "6000-60FF" -> expandGroup [0x6000,0x6002..0x60FF]
    "5000-50FF" -> expandGroup [0x5000,0x5002..0x50FF]
    "7F00-7FFF" -> expandGroup [0x7F00,0x7F02..0x7FFF]
    _    -> if isNothing (tagNbr grp)  || isNothing (tagNbr ele)
              then [DM.Skip] --One of the tags isn't a numeric tag that we'll handle
              else [DM.DictElement (fromJust (tagNbr grp)) (fromJust (tagNbr ele)) vrv nam vmv ver]
  where tagNbr::BS.ByteString-> Maybe Word16
        tagNbr g = let grpNbr' = readHex (BSC.unpack g)
                 in case grpNbr' of
                     [] -> Nothing
                     _  -> Just $ fst (head grpNbr')
        expandGroup = map mkDataElement
        mkDataElement grp'  = DM.DictElement grp' (fromJust $ tagNbr ele) vrv nam vmv ver

