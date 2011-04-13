-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Data.Char
import Data.List
import Data.Function

import System.Environment

import qualified Data.ByteString.Char8 as C
import qualified Crypto.Hash.SHA1 as H

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ battle $ take 2 args

salt = ".NaCl.$^d43lwz;)3s.optimize.this"

-- Perform a complete battle between monsters
battle monsters = printWinner $ getWinner $ zip (map hash $ mangle monsters) monsters

printWinner :: Either String String -> String
printWinner =
    either (\w -> "The " ++ w ++ " refuses to fight itself!")
           (\w -> "The " ++ w ++ " wins!")

getWinner :: [(C.ByteString, String)] -> Either String String
getWinner entries
    | all (== fst (head entries)) (map fst entries) = Left $ firstNameFrom entries
    | otherwise = Right $ firstNameFrom $ sortBy (compare `on` fst) entries
    where firstNameFrom = trim . snd . head

-- String manipulations
hash :: String -> C.ByteString
hash = H.hash . C.pack

mangle :: [String] -> [String]
mangle xs =
    let cxs = map clean xs
    in map (++salt) $ zipWith (++) cxs (reverse cxs)

clean = trim . map toLower

trim :: String -> String
trim =
    let f = reverse . dropWhile isSpace
    in f . f
