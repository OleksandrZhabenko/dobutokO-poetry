-- |
-- Module      :  Main
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to order the 7 or less Ukrainian words (or their concatenations) 
-- to obtain (to some extent) suitable for poetry or music text.

module Main where

import Control.Exception (onException)
import System.IO
import DobutokO.Poetry.General (uniqNPoeticalG)
import DobutokO.Poetry.Data (PreApp(K))
import DobutokO.Poetry.Norms (norm4,norm5,norm51,norm513,norm6)
import System.Environment (getArgs)
import Melodics.Executable (recFileName, printInfoF, rawToSoundFile)
import Melodics.Ukrainian (appendS16LEFile, convertToProperUkrainian)
import EndOfExe (showE)
import Data.Maybe (fromJust,isJust)

-- | The first command line argument specifies which function to run. If given \"4\" it runs 'uniqNPoeticalG' 'norm4', \"51\" it runs 'uniqNPoeticalG' 'norm51', 
-- \"513\" it runs 'uniqNPoeticalG' 'norm513', \"6\" it runs 'uniqNPoeticalG' 'norm6', otherwise 'uniqNPoeticalG' 'norm5'. 
-- The second command line argument is an 'Int' number of the needed printed variants. The next 7 
-- are treated as the Ukrainian words to be ordered accordingly to the norm. For more information, please, refer to the documentation for the abovementioned 
-- functions. 
-- 
-- Afterwards, you can generate a sounding using 'workWithInput' in the \".wav\" format.
main :: IO ()
main = do 
  args <- getArgs
  let arg0 = concat . take 1 $ args
      arg01 = concat . drop 1 . take 2 $ args
      word1s = unwords . drop 2 $ args
  arg1 <- onException (do 
    let aa = read arg01::Int
    return aa) (return 10)
  case (arg0,arg1) of
   ("4",n) -> uniqNPoeticalG K n norm4 word1s
   ("51",n) -> uniqNPoeticalG K n norm51 word1s 
   ("513",n) -> uniqNPoeticalG K n norm513 word1s
   ("6",n) -> uniqNPoeticalG K n norm6 word1s 
   ~(_,n) -> uniqNPoeticalG K n norm5 word1s 
  putStrLn "What string would you like to record as a Ukrainian text sounding by mmsyn6ukr package? "
  str <- getLine
  nameAndRec str

-- | Is used to specify a name for the recorded sounding for the selected text and to record it.
nameAndRec :: String -> IO ()
nameAndRec str = do 
  name <- recFileName
  withBinaryFile (name ++ ".raw") AppendMode (appendS16LEFile (convertToProperUkrainian str))
  putStrLn "The .raw file was created by the program. If there is SoX installed then it will run further. "
  let ts = showE "sox" 
  if isJust ts
    then rawToSoundFile "" name (fromJust ts)
    else printInfoF
