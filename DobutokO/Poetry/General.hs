-- |
-- Module      :  DobutokO.Poetry.General
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Helps to order the 7 or less Ukrainian words (or their concatenations) 
-- to obtain (to some extent) suitable for poetry or music text. 
-- Generalization of the functionality in the 'DobutokO.Poetry.Basic' 
-- and 'DobutokO.Poetry' modules. The functions can only print the needed 
-- strings or also return tha data needed to interconnect and link it with 
-- other functions. There is also a possibility to use prepending and 
-- postpending 'String' in the 'PreApp' data type so that they are added 
-- respectively to the beginning or to the end of the strings.

module DobutokO.Poetry.General where

import Data.Maybe (fromJust)
import Data.Char (isPunctuation)
import qualified Data.Vector as V
import String.Ukrainian.UniquenessPeriods (uniquenessPeriods)
import DobutokO.Poetry.Norms
import DobutokO.Poetry.Norms.Extended
import DobutokO.Poetry.Auxiliary
import DobutokO.Poetry.UniquenessPeriodsG
import DobutokO.Poetry.StrictV
import DobutokO.Poetry.Data

-- | Prints the maximum element with respect of the @k@ norms (the most significant of which is the rightest one, then to the left less significant etc.), 
-- which is given as the first argument. The last norm is the first element in the 'V.Vector' of norms (@[Int] -> Int@). 
uniqInMaxPoeticalN :: Int -> V.Vector ([Int] -> Int) -> UniqG -> IO UniqG
uniqInMaxPoeticalN k vN x = do
  inner1 k vN x >>= \(fsT,x) -> 
    if isU x then return (U (V.filter (\(xs,_,_) -> xs /= fsT) . snd . get2 $ x))
    else return (UL ((\(v1,v2) -> ((V.toList . V.map (filter (not . isPunctuation) . lastFrom3) $ v1) ++ (fromJust . fst . get2 $ x),v2)) . 
      V.unstablePartition (\(xs,_,_) -> xs == fsT) . snd . get2 $ x))
{-# INLINE uniqInMaxPoeticalN #-}

-- | Is used internally in the 'uniqInMaxPoeticalN' to reduce duplication.
inner1 :: Int -> V.Vector ([Int] -> Int) -> UniqG -> IO ([Int],UniqG)
inner1 k vN x = do 
  let uniq = uniqMaxPoeticalGNV k vN x
  let fsT = (\(ys,_,_) -> ys) uniq
  putStr (filter (not . isPunctuation) . lastFrom3 $ uniq) >> putStrLn ""
  return (fsT,x)
{-# INLINE inner1 #-}  

-- | Variant of 'uniqInMaxPoticalN' where all the elements in the norms 'V.Vector' are used as norms from right to left.
uniqInMaxPoeticalNL :: V.Vector ([Int] -> Int) -> UniqG -> IO UniqG
uniqInMaxPoeticalNL vN x = uniqInMaxPoeticalN (V.length vN) vN x
{-# INLINE uniqInMaxPoeticalNL #-}

-- | Generalized variant of the 'uniqInMaxPoeticalN' with usage of the several norms and all the information is printed on the same line. 
uniqInMaxPoeticalNLine :: Int -> V.Vector ([Int] -> Int) -> UniqG -> IO UniqG
uniqInMaxPoeticalNLine k vN x = do
  inner2 k vN x >>= \(fsT,x) -> 
    if isU x then return (U (V.filter (\(xs,_,_) -> xs /= fsT) . snd . get2 $ x))
    else return (UL ((\(v1,v2) -> ((V.toList . V.map (filter (not . isPunctuation) . lastFrom3) $ v1) ++ (fromJust . fst . get2 $ x),v2)) . 
      V.unstablePartition (\(xs,_,_) -> xs == fsT) . snd . get2 $ x))
{-# INLINE uniqInMaxPoeticalNLine #-}

-- | Is used internally in the 'uniqInMaxPoeticalNLine' to reduce duplication.
inner2 :: Int -> V.Vector ([Int] -> Int) -> UniqG -> IO ([Int],UniqG)
inner2 k vN x = do 
  let uniq = uniqMaxPoeticalGNV k vN x
  let fsT = (\(ys,_,_) -> ys) uniq
  putStr (filter (not . isPunctuation) . lastFrom3 $ uniq) >> putStr " "
  return (fsT,x)
{-# INLINE inner2 #-}

-- | Variant of 'uniqInMaxPoticalNLine' where all the elements in the norms 'V.Vector' are used as norms from right to left.
uniqInMaxPoeticalNLineL :: V.Vector ([Int] -> Int) -> UniqG -> IO UniqG
uniqInMaxPoeticalNLineL vN = uniqInMaxPoeticalNLine (V.length vN) vN
{-# INLINE uniqInMaxPoeticalNLineL #-}

-- | Prints @n@ (given as the first argument) maximum elements with respect to the several norms (their quantity is the second argument) starting 
-- from the right to the left. The last norm is the first element in the 'V.Vector' of norms (@[Int] -> Int@). 
uniqNPoeticalN :: Int -> Int -> V.Vector ([Int] -> Int) -> UniqG -> IO ()
uniqNPoeticalN n k vN y  
 | n <= 0 = return ()
 | compare (V.length . snd . get2 $ y) n == LT = V.mapM_ (\x -> putStr (filter (not . isPunctuation) . lastFrom3 $ x) >> putStrLn "" ) . snd . get2 $ y
 | otherwise = (uniqInMaxPoeticalN k vN y >>= uniqNPoeticalN (n - 1) k vN)
{-# INLINE uniqNPoeticalN #-}

-- | Variant of 'uniqNPoeticalN' where all the elements in the norms 'V.Vector' are used as norms from right to left.
uniqNPoeticalNL :: Int -> V.Vector ([Int] -> Int) -> UniqG -> IO ()
uniqNPoeticalNL n vN = uniqNPoeticalN n (V.length vN) vN
{-# INLINE uniqNPoeticalNL #-}

-- | Variant of the 'uniqNPoeticalN' with its output being printed on the same line.
uniqNPoeticalNLine :: Int -> Int -> V.Vector ([Int] -> Int) -> UniqG -> IO ()
uniqNPoeticalNLine n k vN y
 | n <= 0 = putStrLn ""
 | compare (V.length . snd . get2 $ y) n == LT = 
    (V.mapM_ (\x -> putStr (filter (not . isPunctuation) . lastFrom3 $ x) >> putStr " " ) . snd . get2 $ y)  >> putStrLn ""
 | otherwise = (uniqInMaxPoeticalNLine k vN y >>= uniqNPoeticalNLine (n - 1) k vN)
{-# INLINE uniqNPoeticalNLine #-}
 
-- | Variant of 'uniqNPoeticalNLine' where all the elements in the norms 'V.Vector' are used as norms from right to left.
uniqNPoeticalNLineL :: Int -> V.Vector ([Int] -> Int) -> UniqG -> IO ()
uniqNPoeticalNLineL n vN = uniqNPoeticalNLine n (V.length vN) vN
{-# INLINE uniqNPoeticalNLineL #-}

-- | Prints @n@ (given as the first argument) maximum elements with respect to the several norms (their quantity is the second argument) starting 
-- from the right to the left. The last norm is the first element in the 'V.Vector' of norms (@[Int] -> Int@). Contrary to its pair function 
-- 'uniqNPoeticalN' returns then the rest of the given 'V.Vector' 'Uniqueness' after filtering the printed elements 'String'.
uniqNPoeticalVN :: Int -> Int -> V.Vector ([Int] -> Int) -> UniqG -> IO UniqG
uniqNPoeticalVN n k vN y
 | n <= 0 || compare (V.length . snd . get2 $ y) n == LT = return y
 | otherwise = (uniqInMaxPoeticalN k vN y >>= uniqNPoeticalVN (n - 1) k vN)
{-# INLINE uniqNPoeticalVN #-}

-- | Variant of 'uniqNPoeticalVN' where all the elements in the norms 'V.Vector' are used as norms from right to left.
uniqNPoeticalVNL :: Int -> V.Vector ([Int] -> Int) -> UniqG -> IO UniqG
uniqNPoeticalVNL n vN = uniqNPoeticalVN n (V.length vN) vN
{-# INLINE uniqNPoeticalVNL #-}

-- | The function evaluates the 'V.Vector' of 'Uniqueness' elements to retrieve the possibly maximum element in it with respect to the order 
-- and significance (principality)  of the norms being evaluated. The most significant and principal is the norm, which index in the 'V.Vector' of them is 
-- the 'Int' argument of the function minus 1, then less significant is the next to the left norm and so on. Is similar to 'DobutokO.Poetry.uniqMaxPoeticalGN' 
-- function.
uniqMaxPoeticalGNV :: Int -> V.Vector ([Int] -> Int) ->  UniqG -> Uniqueness
uniqMaxPoeticalGNV k vN y
 | compare k (V.length vN) == GT = error "DobutokO.Poetry.General.uniqMaxPoeticalGNV: undefined for that amount of norms. "
 | compare k 0 == GT =
   let maxK = V.maximumBy (\(_,vN0,_) (_,vN1,_) -> compare (V.unsafeIndex vN0 (k - 1)) (V.unsafeIndex vN1 (k - 1))) . snd . get2 $ y
       vK = V.filter (\(_,vN2,_) -> V.unsafeIndex vN2 (k - 1) == ((\(_,vNk,_) -> V.unsafeIndex vNk (k - 1)) maxK)) . snd . get2 $ y in
         if isU y then uniqMaxPoeticalGNV (k - 1) (V.unsafeSlice 0 (V.length vN - 1) vN) (U vK)
         else uniqMaxPoeticalGNV (k - 1) (V.unsafeSlice 0 (V.length vN - 1) vN) (UL (fromJust . fst . get2 $ y,vK))
 | otherwise = V.maximumBy (\(_,vN0,_) (_,vN1,_) -> compare (V.unsafeIndex vN0 0) (V.unsafeIndex vN1 0)) . snd . get2 $ y
{-# INLINE uniqMaxPoeticalGNV #-}

-- | Variant of 'uniqMaxPoeticalGNV' where all the elements in the norms 'V.Vector' are used as norms from right to left.
uniqMaxPoeticalGNVL :: V.Vector ([Int] -> Int) ->  UniqG -> Uniqueness
uniqMaxPoeticalGNVL vN = uniqMaxPoeticalGNV (V.length vN) vN
{-# INLINE uniqMaxPoeticalGNVL #-}

---------------------------------------------------------------------------------

-- | Returns the 'V.Vector' of all possible permutations of the 'String' that represent the Ukrainian text and the linked information with them for 
-- analysis  with usage of several norms (instead of one). They constitute a 'V.Vector' of functions 
-- @norm :: [Int] -> Int@. So the inner vector in the each resulting 'Uniqueness' has the same length as the vector of norms. 
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
uniquenessVariantsGN :: Preapp -> V.Vector ([Int] -> Int) -> String -> V.Vector Uniqueness
uniquenessVariantsGN (PA ts us) vN = uniquenessVariants2GNP ts us vN (uniquenessPeriods)
uniquenessVariantsGN K vN = uniquenessVariants2GN vN (uniquenessPeriods)
{-# INLINE uniquenessVariantsGN #-}

-- | A variant of the 'uniqMaxPoetical2GN' with the several norms given as a 'V.Vector' of functions and an 'Int' parameter. The function evaluates 
-- the generated 'V.Vector' of 'Uniqueness' elements to retrieve the possibly maximum element in it with respect to the order and significance (principality) 
-- of the norms being evaluated. The most significant and principal is the norm, which index in the 'V.Vector' of them is the 'Int' argument of the function 
-- minus 1, then less significant is the next to the left norm and so on.
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
uniqMaxPoeticalGN :: Preapp -> Int -> V.Vector ([Int] -> Int) ->  String -> Uniqueness
uniqMaxPoeticalGN x k vN = uniqMaxPoetical2GN x k vN (uniquenessPeriods)
{-# INLINE uniqMaxPoeticalGN #-}

-- | Variant of 'uniqMaxPoeticalGN' where all the elements in the norms 'V.Vector' are used as norms from right to left.
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
uniqMaxPoeticalGNL :: Preapp -> V.Vector ([Int] -> Int) ->  String -> Uniqueness
uniqMaxPoeticalGNL x vN = uniqMaxPoeticalGN x (V.length vN) vN
{-# INLINE uniqMaxPoeticalGNL #-}

-- | A variant of the 'uniqNPoeticalGN' with only one norm.
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
uniqNPoeticalG :: Preapp -> Int -> ([Int] -> Int) -> String -> IO ()
uniqNPoeticalG x n g = uniqNPoeticalGN x n 1 (V.singleton g)
{-# INLINE uniqNPoeticalG #-}

-- | A variant of the 'uniqNPoeticalG' function with the @n@ equal to 10.
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
uniq10PoeticalG :: Preapp -> ([Int] -> Int) -> String -> IO ()
uniq10PoeticalG x = uniqNPoeticalG x 10
{-# INLINE uniq10PoeticalG #-}

-- | A variant of 'uniq10PoeticalG' with the 'norm4' applied. The list is (according to some model, not universal, but a reasonable one in the most cases) the 
-- most suitable for intonation changing and, therefore, for the accompaniment of the highly changable or variative melody. 
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
uniq10Poetical4 :: Preapp -> String -> IO ()
uniq10Poetical4 x = uniq10PoeticalG x norm4
{-# INLINE uniq10Poetical4 #-}

-- | A variant of 'uniq10PoeticalG' with the 'norm5' applied. The list is (according to some model, not universal, but a reasonable one in the most cases) the 
-- most suitable for rhythmic speech and two-syllabilistic-based poetry. Therefore, it can be used to create a poetic composition or to emphasize some 
-- thoughts. 
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
uniq10Poetical5 :: Preapp -> String -> IO ()
uniq10Poetical5 x = uniq10PoeticalG x norm5
{-# INLINE uniq10Poetical5 #-}

-- | A variant of the 'uniqNPoetical2GN' with the conversion (\"uniquenessPeriods\" function) function 'uniquenessPeriods'.
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
uniqNPoeticalGN :: Preapp -> Int -> Int -> V.Vector ([Int] -> Int) -> String -> IO ()
uniqNPoeticalGN x n k vN = uniqNPoetical2GN x n k vN (uniquenessPeriods)
{-# INLINE uniqNPoeticalGN #-}

-- | Variant of 'uniqNPoeticalGN' where all the elements in the norms 'V.Vector' are used as norms from right to left. 
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
uniqNPoeticalGNL :: Preapp -> Int -> V.Vector ([Int] -> Int) -> String -> IO ()
uniqNPoeticalGNL x n vN = uniqNPoetical2GNL x n vN (uniquenessPeriods)
{-# INLINE uniqNPoeticalGNL #-}

-- | Generalized variant of the 'uniqNPoeticalVG' with usage of several norms. 
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- To specify whether the function returns also data suitable for usage with other functions or just usable mostly for printing it uses also a before 'String' 
-- argument also 'UniqG' one with the 'U' data constructor corresponding to the printing (mostly) and 'UL' to possible reusage of data.
uniqNPoeticalVGN :: Preapp -> Int -> Int -> V.Vector ([Int] -> Int) -> UniqG -> String -> IO UniqG
uniqNPoeticalVGN x n k vN = uniqNPoetical2VGN x n k vN (uniquenessPeriods)
{-# INLINE uniqNPoeticalVGN #-}

-- | Variant of 'uniqNPoeticalVGN' where all the elements in the norms 'V.Vector' are used as norms from right to left.
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- To specify whether the function returns also data suitable for usage with other functions or just usable mostly for printing it uses also a before 'String' 
-- argument also 'UniqG' one with the 'U' data constructor corresponding to the printing (mostly) and 'UL' to possible reusage of data.
uniqNPoeticalVGNL :: Preapp -> Int -> V.Vector ([Int] -> Int) -> UniqG -> String -> IO UniqG
uniqNPoeticalVGNL x n vN = uniqNPoetical2VGN x n (V.length vN) vN (uniquenessPeriods)
{-# INLINE uniqNPoeticalVGNL #-}

-- | The function evaluates 
-- the generated 'V.Vector' of 'Uniqueness' elements to retrieve the possibly maximum element in it with respect to the order and significance (principality) 
-- of the norms being evaluated. The most significant and principal is the norm, which index in the 'V.Vector' of them is the 'Int' argument of the function 
-- minus 1, then less significant is the next to the left norm and so on.
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
uniqMaxPoetical2GN :: Preapp -> Int -> V.Vector ([Int] -> Int) ->  (String -> [Int]) -> String -> Uniqueness
uniqMaxPoetical2GN rr k vN g xs
 | compare k (V.length vN) == GT = error "DobutokO.Poetry.General.uniqMaxPoetical2GN: undefined for that amount of norms. "
 | compare k 0 == GT =
   let vM = uniquenessVariants2GNP (get1m rr) (get2m rr) vN g xs
       maxK = V.maximumBy (\(_,vN0,_) (_,vN1,_) -> compare (V.unsafeIndex vN0 (k - 1)) (V.unsafeIndex vN1 (k - 1))) vM
       vK = V.filter (\(_,vN2,_) -> V.unsafeIndex vN2 (k - 1) == ((\(_,vNk,_) -> V.unsafeIndex vNk (k - 1)) maxK)) vM in
         uniqMaxPoeticalGNV (k - 1) (V.unsafeSlice 0 (V.length vN - 1) vN) (U vK)
 | otherwise = V.maximumBy (\(_,vN0,_) (_,vN1,_) -> compare (V.unsafeIndex vN0 0) (V.unsafeIndex vN1 0)) . uniquenessVariantsGN rr vN $ xs

-- | Variant of 'uniqMaxPoetical2GN' where all the elements in the norms 'V.Vector' are used as norms from right to left.
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
uniqMaxPoetical2GNL :: Preapp -> V.Vector ([Int] -> Int) ->  (String -> [Int]) -> String -> Uniqueness
uniqMaxPoetical2GNL rr vN = uniqMaxPoetical2GN rr (V.length vN) vN
{-# INLINE uniqMaxPoetical2GNL #-}
  
-- | Prints @n@ (given as the first 'Int' argument) maximum elements with respect to the several norms (their quantity is the second 'Int' argument) starting 
-- from the right to the left. The last norm is the first element in the 'V.Vector' of norms (@[Int] -> Int@). 
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
uniqNPoetical2GN :: Preapp -> Int -> Int -> V.Vector ([Int] -> Int) -> (String -> [Int]) -> String -> IO ()
uniqNPoetical2GN rr n k vN g xs
 | n <= 0 = return ()
 | otherwise = do
   let v = uniquenessVariants2GNP (get1m rr) (get2m rr) vN g xs
   if compare (V.length v) n == LT
     then V.mapM_ (\x -> putStr ((filter (not . isPunctuation) . lastFrom3 $ x)) >> putStrLn "" ) v
     else (uniqInMaxPoeticalN k vN (U v) >>= uniqNPoeticalN (n - 1) k vN)

-- | Variant of 'uniqNPoetical2GN' where all the elements in the norms 'V.Vector' are used as norms from right to left.
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
uniqNPoetical2GNL :: Preapp -> Int -> V.Vector ([Int] -> Int) -> (String -> [Int]) -> String -> IO ()
uniqNPoetical2GNL rr n vN = uniqNPoetical2GN rr n (V.length vN) vN
{-# INLINE uniqNPoetical2GNL #-}
 
-- | Generalized variant of the 'uniqNPoeticalG' with usage of the several norms, but prints its output on the same line. 
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
uniqNPoetical2GNLine :: Preapp -> Int -> Int -> V.Vector ([Int] -> Int) -> (String -> [Int]) -> String -> IO ()
uniqNPoetical2GNLine rr n k vN g xs
 | n <= 0 = putStrLn ""
 | otherwise = do
   let v = uniquenessVariants2GNP (get1m rr) (get2m rr) vN g xs
   if compare (V.length v) n == LT
     then V.mapM_ (\x -> putStr ((filter (not . isPunctuation) . lastFrom3 $ x)) >> putStr " " ) v >> putStrLn ""
     else (uniqInMaxPoeticalNLine k vN (U v) >>= uniqNPoeticalNLine (n - 1) k vN)

-- | Variant of 'uniqNPoetical2GNLine' where all the elements in the norms 'V.Vector' are used as norms from right to left.
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
uniqNPoetical2GNLineL :: Preapp -> Int -> V.Vector ([Int] -> Int) -> (String -> [Int]) -> String -> IO ()
uniqNPoetical2GNLineL rr n vN = uniqNPoetical2GNLine rr n (V.length vN) vN
{-# INLINE uniqNPoetical2GNLineL #-}
 
-- | Prints @n@ (given as the first 'Int' argument) maximum elements with respect to the several norms (their quantity is the second 'Int' argument) starting 
-- from the right to the left. The last norm is the first element in the 'V.Vector' of norms (@[Int] -> Int@). Contrary to its pair function 
-- 'uniqNPoetical2GN' returns then the rest of the given 'V.Vector' 'Uniqueness' after filtering the printed elements 'String'.
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- To specify whether the function returns also data suitable for usage with other functions or just usable mostly for printing it uses also a before 'String' 
-- argument also 'UniqG' one with the 'U' data constructor corresponding to the printing (mostly) and 'UL' to possible reusage of data.
uniqNPoetical2VGN :: Preapp -> Int -> Int -> V.Vector ([Int] -> Int) -> (String -> [Int]) -> UniqG -> String -> IO UniqG
uniqNPoetical2VGN rr n k vN g y xs
 | n <= 0 = if isU y then return (U V.empty) else return (UL ([],V.empty))
 | otherwise = do
   let v = uniquenessVariants2GNP (get1m rr) (get2m rr) vN g xs
   if compare (V.length v) n == LT 
     then if isU y then return (U v) else return (UL ([],v)) 
     else if isU y then uniqNPoeticalVN n k vN (U v) else uniqNPoeticalVN n k vN (UL ([],v))

-- | Variant of 'uniqNPoetical2VGN' where all the elements in the norms 'V.Vector' are used as norms from right to left.
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- To specify whether the function returns also data suitable for usage with other functions or just usable mostly for printing it uses also a before 'String' 
-- argument also 'UniqG' one with the 'U' data constructor corresponding to the printing (mostly) and 'UL' to possible reusage of data.
uniqNPoetical2VGNL :: Preapp -> Int -> V.Vector ([Int] -> Int) -> (String -> [Int]) -> UniqG -> String -> IO UniqG
uniqNPoetical2VGNL rr n vN = uniqNPoetical2VGN rr n (V.length vN) vN
{-# INLINE uniqNPoetical2VGNL #-}
 
-- | Variant of the 'uniqNPoetical2GN', which uses as a function 'uniquenessPeriods2' with the first argument equal to the first 'Int' argument.
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
uniqNPoeticalUGN_ :: Preapp -> Int -> Int -> Int -> V.Vector ([Int] -> Int) -> String -> IO ()
uniqNPoeticalUGN_ rr x n k vN = uniqNPoetical2GN rr n k vN (uniquenessPeriods2 x)
{-# INLINE uniqNPoeticalUGN_ #-}

-- | Variant of 'uniqNPoeticalUGN_' where all the elements in the norms 'V.Vector' are used as norms from right to left.
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
uniqNPoeticalUGNL_ :: Preapp -> Int -> Int -> V.Vector ([Int] -> Int) -> String -> IO ()
uniqNPoeticalUGNL_ rr x n vN = uniqNPoetical2GNL rr n vN (uniquenessPeriods2 x)
{-# INLINE uniqNPoeticalUGNL_ #-}

-- | Variant of the 'uniqNPoetical2VGN', which uses as a function 'uniquenessPeriods2' with the first argument equal to the first 'Int' argument.
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- To specify whether the function returns also data suitable for usage with other functions or just usable mostly for printing it uses also a before 'String' 
-- argument also 'UniqG' one with the 'U' data constructor corresponding to the printing (mostly) and 'UL' to possible reusage of data.
uniqNPoeticalUGN :: Preapp -> Int -> Int -> Int -> V.Vector ([Int] -> Int) -> UniqG -> String -> IO UniqG
uniqNPoeticalUGN rr x n k vN = uniqNPoetical2VGN rr n k vN (uniquenessPeriods2 x)
{-# INLINE uniqNPoeticalUGN #-}

-- | Variant of 'uniqNPoeticalUGN' where all the elements in the norms 'V.Vector' are used as norms from right to left.
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- To specify whether the function returns also data suitable for usage with other functions or just usable mostly for printing it uses also a before 'String' 
-- argument also 'UniqG' one with the 'U' data constructor corresponding to the printing (mostly) and 'UL' to possible reusage of data.
uniqNPoeticalUGNL :: Preapp -> Int -> Int -> V.Vector ([Int] -> Int) -> UniqG -> String -> IO UniqG
uniqNPoeticalUGNL rr x n vN = uniqNPoetical2VGN rr n (V.length vN) vN (uniquenessPeriods2 x)
{-# INLINE uniqNPoeticalUGNL #-}

-- | Variant of the 'uniqNPoeticalUGN_', which uses as a single norm 'norm51'.
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
uniqNPoeticalUGN51_ :: Preapp -> Int -> Int -> String -> IO ()
uniqNPoeticalUGN51_ rr x n = uniqNPoeticalUGN_ rr x n 1 (V.singleton norm51)
{-# INLINE uniqNPoeticalUGN51_ #-}

-- | Variant of the 'uniqNPoeticalUGN', which uses as a single norm 'norm51'.
-- In contrast to the the same named function from the 'DobutokO.Poetry' module uses in the processment prepending and appending 'String' that are lifted (if any) 
-- to the 'PreApp' data type with the respective constuctors. The first one is prepended and the second one is appended to the processed 'String' to 
-- be processed with it. This allows to create more connection with the previous and postpending text.
-- To specify whether the function returns also data suitable for usage with other functions or just usable mostly for printing it uses also a before 'String' 
-- argument also 'UniqG' one with the 'U' data constructor corresponding to the printing (mostly) and 'UL' to possible reusage of data.
uniqNPoeticalUGN51 :: Preapp -> Int -> Int -> UniqG -> String -> IO UniqG
uniqNPoeticalUGN51 rr x n = uniqNPoeticalUGN rr x n 1 (V.singleton norm51)
{-# INLINE uniqNPoeticalUGN51 #-}
  
