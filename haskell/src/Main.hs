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
import System.Exit
import System.IO

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
     ,Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
        "Show help"
    ]

main :: IO ()
main = do
    rawArgs <- getArgs

    let (actions, args, errors) = getOpt RequireOrder options rawArgs
    opts <- foldl (>>=) (return defaultOpts) actions

    case optBattle opts of
        Single      -> putStrLn $ printWinner $ battle (head args) (args !! 1)
        Cage        -> matchgraph $ cagematch args
        Tournament  -> print $ tournament $ initTournament args

salt = ".NaCl.$^d43lwz;)3s.optimize.this"

-- Single battle functions
printWinner :: Outcome -> String
printWinner =
    either (\w -> "The " ++ w ++ " refuses to fight itself!")
           (\w -> "The " ++ winner w ++ " wins!")

-- Cagematch functions
matchgraph :: [Victory] -> IO ()
matchgraph = printGraph . mkBattleGraph

cagematch :: [String] -> [Victory]
cagematch ms = (rights . nub) $ battle <$> ms <*> ms

-- Tournament functions
initTournament :: [String] -> [Victory]
initTournament = map (\x -> Victory x x)

tournament :: [Victory] -> [[Victory]]
tournament []  = []
tournament [x] = []
tournament xs  =
    let
        round = tRound xs
    in round : tournament round

tRound :: [Victory] -> [Victory]
tRound = map (either (\x -> Victory x x) id . (uncurry battle)) . pairify . map winner

pairify :: [a] -> [(a, a)]
pairify []        = []
pairify [x]       = [(x, x)]
pairify (x:x':xs) = (x, x') : pairify xs

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

mkBattleGraph :: [Victory] -> G.Gr String ()
mkBattleGraph vs =
    let
        edges = map (\v -> (loser v, winner v, ()))
        ms = nub $ map loser vs ++ map winner vs
    in fst $ G.mkMapGraph ms (edges vs)

-- General battle functions
battle :: String -> String -> Outcome
battle x y
    | x == y    = Left x
    | x' > y'   = Right $ Victory {winner = trim x, loser = trim y}
    | otherwise = Right $ Victory {winner = trim y, loser = trim x}
    where
        cx = clean x
        cy = clean y
        x' = hash $ cx ++ cy ++ salt
        y' = hash $ cy ++ cx ++ salt

-- String manipulations
hash :: String -> C.ByteString
hash = H.hash . C.pack

clean = map toLower . trim

trim :: String -> String
trim =
    let f = reverse . dropWhile isSpace
    in f . f
