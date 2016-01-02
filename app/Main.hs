module Main where

import Data.DICOM.Dictionary
import qualified Data.DICOM.Model.Dictionary as DM
import Numeric
import Paths_dicom_dictionary
main :: IO ()
main = do
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
