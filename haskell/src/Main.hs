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
import Control.Monad

import Data.Char
import Data.Either
import Data.Function
import Data.List

import qualified Data.Graph.Inductive as G
import qualified Data.GraphViz as GV

import System.Console.GetOpt
import System.Environment

import qualified Data.ByteString.Char8 as C
import qualified Crypto.Hash.SHA1 as H

data Victory = Victory { winner :: String, loser :: String } deriving (Show, Eq)
type Outcome = Either String Victory

-- http://www.haskell.org/haskellwiki/High-level_option_handling_with_GetOpt
data Options = Options { optCagematch :: Bool }

startOptions :: Options
startOptions = Options { optCagematch = False }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option "c" ["cagematch"]
        (NoArg
            (\opt -> return opt { optCagematch = True }))
        "Run cagematch instead of direct competition."
    ]

main :: IO ()
main = do
    rawArgs <- getArgs

    let (actions, args, errors) = getOpt RequireOrder options rawArgs
    opts <- foldl (>>=) (return startOptions) actions

    let Options { optCagematch = doCagematch } = opts

    when doCagematch $ void $ outgraph args
    when (not doCagematch) $ putStrLn $ battle $ take 2 args

salt = ".NaCl.$^d43lwz;)3s.optimize.this"

outgraph :: [String] -> IO ()
outgraph ms = do
    result <- GV.runGraphvizCommand GV.Circo (dotgraph ms) GV.Png "creaturebattle.png"
    either putStrLn (putStr . const "") result

-- Perform a cagematch between monsters, and make a dot string out of it
dotgraph ms = GV.graphToDot params $ graphCM ms $ cagematch ms
    where
        params = GV.nonClusteredParams { GV.fmtNode = label }
        label (_, s) = [GV.Label $ GV.StrLabel s]

-- Perform a two-way battle between monsters
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
    in nub $ rights $ cm <$> ms <*> ms

graphCM :: [String] -> [Victory] -> G.Gr String ()
graphCM ms vs =
    let edges = map (\v -> (winner v, loser v, ()))
    in fst $ G.mkMapGraph ms (edges vs)

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
