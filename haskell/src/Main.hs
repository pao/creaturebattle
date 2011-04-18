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

import Control.Applicative

import Data.Char
import Data.List
import Data.Either
import Data.Function

import System.Environment

import qualified Data.ByteString.Char8 as C
import qualified Crypto.Hash.SHA1 as H

data Victory = Victory { winner :: String, loser :: String } deriving (Show)
type Outcome = Either String Victory

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ battle $ take 2 args

salt = ".NaCl.$^d43lwz;)3s.optimize.this"

-- Perform a complete battle between monsters
battle = printWinner . getWinner . battlepair

battlepair :: [String] -> [(C.ByteString, String)]
battlepair monsters =
    let tm = map trim monsters
    in zip (map hash $ mangle tm) tm

printWinner :: Outcome -> String
printWinner =
    either (\w -> "The " ++ w ++ " refuses to fight itself!")
           (\w -> "The " ++ winner w ++ " wins!")

getWinner :: [(C.ByteString, String)] -> Outcome
getWinner entries
    | all (== fst (head entries)) (map fst entries) = Left $ snd $ head $ entries
    | otherwise = Right $ Victory (head fight) (head $ tail fight)
    where fight = snd $ unzip $ sortBy (compare `on` fst) entries

cagematch :: [String] -> [Victory]
cagematch ms =
    let cm x y = getWinner $ battlepair [x, y]
    in rights $ cm <$> ms <*> ms

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
