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

data BattleMode = Single | Cage | Tournament

-- http://www.haskell.org/haskellwiki/High-level_option_handling_with_GetOpt
data Options = Options { optBattle :: BattleMode }

defaultOpts :: Options
defaultOpts = Options { optBattle = Single }

options :: [OptDescr (Options -> IO Options)]
options =
    [
      Option "c" ["cagematch"]
        (NoArg
            (\opt -> return opt { optBattle = Cage }))
        "Run cagematch instead of direct competition."
     ,Option "t" ["tournament"]
        (NoArg
            (\opt -> return opt { optBattle = Tournament }))
        "Run bracket tournament instead of direct competition."
    ]

main :: IO ()
main = do
    rawArgs <- getArgs

    let (actions, args, errors) = getOpt RequireOrder options rawArgs
    opts <- foldl (>>=) (return defaultOpts) actions

    case optBattle opts of
        Single      -> putStrLn $ printBattle $ take 2 args
        Cage        -> matchgraph cagematch args
        Tournament  -> return ()

salt = ".NaCl.$^d43lwz;)3s.optimize.this"

-- Single battle functions
printBattle = printWinner . battle

battle = getWinner . battlepair

printWinner :: Outcome -> String
printWinner =
    either (\w -> "The " ++ w ++ " refuses to fight itself!")
           (\w -> "The " ++ winner w ++ " wins!")

-- Multi-battle functions
matchgraph :: ([String] -> [Victory]) -> [String] -> IO ()
matchgraph f ms = printGraph . mkBattleGraph ms $ f ms

cagematch :: [String] -> [Victory]
cagematch ms =
    let cm x y = getWinner $ battlepair [x, y]
    in nub $ rights $ cm <$> ms <*> ms

-- Graph generation functions
printGraph = graphOut . dotgraph

graphOut :: GV.DotGraph G.Node -> IO ()
graphOut dot = do
    result <- GV.runGraphvizCommand GV.Circo dot GV.Png "creaturebattle.png"
    either putStrLn (return . const ()) result

dotgraph :: G.Gr String a -> GV.DotGraph G.Node
dotgraph = GV.graphToDot params
    where
        params = GV.nonClusteredParams { GV.fmtNode = label }
        label (_, s) = [GV.Label $ GV.StrLabel s]

mkBattleGraph :: [String] -> [Victory] -> G.Gr String ()
mkBattleGraph ms vs =
    let edges = map (\v -> (loser v, winner v, ()))
    in fst $ G.mkMapGraph ms (edges vs)

-- General battle functions
battlepair :: [String] -> [(C.ByteString, String)]
battlepair monsters =
    let tm = map trim monsters
    in zip (map hash $ mangle tm) tm

getWinner :: [(C.ByteString, String)] -> Outcome
getWinner entries
    | all (== fst (head entries)) (map fst entries) = Left $ snd $ head $ entries
    | otherwise = Right $ Victory (head fight) (fight !! 1)
    where fight = snd $ unzip $ sortBy (compare `on` fst) entries

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
