import System.Environment
import System.Exit
import Data.Char
import Data.List
import qualified Data.Trie as T

stopwordsGerman = ["ein", "Ein"]

main :: IO ()
main = do
    args <- getArgs
    print args
    sortFn <- getSortFunction args
    getContents >>= putStrLn . prettyPrint . sortFn . T.asList . analyse . words

getSortFunction :: [String] -> IO ([(String, Int)] -> [(String, Int)])
getSortFunction ("-c":_) = return (sortBy sortByCount)
getSortFunction ("-w":_) = return (sortBy sortByWord)
getSortFunction _        = return id

usage = do
    prog <- getProgName
    putStrLn $ msg prog
    where
        msg p = "Usage: " ++ p ++ " [-hwc] [file]"

exitParseError = exitWith (ExitFailure 2)
die = exitWith (ExitFailure 1)

analyse :: [String] -> T.Trie Int
analyse = listToTrie . map toLowerCase

prettyPrint :: (Show a, Show b) => [(a, b)] -> String
prettyPrint = foldl (\str tuple -> str ++ show tuple ++ "\n") ""

sortByCount :: (String, Int) -> (String, Int) -> Ordering
sortByCount (_, a) (_, b) = compare b a

sortByWord :: (String, Int) -> (String, Int) -> Ordering
sortByWord (a, _) (b, _) = compare a b

toLowerCase :: String -> String
toLowerCase = map toLower

listToTrie :: [String] -> T.Trie Int
listToTrie =
    foldl
    -- increase count by one or insert one if first occurence
    (\t w -> T.insert w (maybe 1 (+ 1) (T.lkup w t)) t)
    T.empty
