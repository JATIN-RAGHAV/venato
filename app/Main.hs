module Main (main) where

import Control.Exception (evaluate)
import Data.Char (isSpace)
import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Ord (Down (..))
import Data.Time (Day, addDays, getZonedTime, localDay, zonedTimeToLocalTime)
import System.Directory (doesFileExist, getHomeDirectory, renameFile)
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStr, stderr)
import Text.Read (readMaybe)

data Task = Task
  { taskId :: Int
  , title  :: String
  , done   :: Bool
  , day    :: Day
  }
  deriving (Show, Read)

-- Storage: one `show`n Task per line in ~/.venato

storePath :: IO FilePath
storePath = (</> ".venato") <$> getHomeDirectory

loadTasks :: IO [Task]
loadTasks = do
  path <- storePath
  exists <- doesFileExist path
  if not exists
    then pure []
    else do
      contents <- readFile path
      _ <- evaluate (length contents) -- read fully so the file can be rewritten
      case traverse readMaybe (lines contents) of
        Just tasks -> pure tasks
        Nothing    -> die ("venato: could not parse " ++ path)

saveTasks :: [Task] -> IO ()
saveTasks tasks = do
  path <- storePath
  let tmp = path ++ ".tmp"
  writeFile tmp (unlines (map show tasks))
  renameFile tmp path

-- Rendering

renderDay :: Day -> Day -> [Task] -> String
renderDay today d tasks = unlines (header : body)
  where
    header = show d ++ if d == today then " (today)" else " (read-only)"
    width = maximum (0 : map (length . show . taskId) tasks)
    line t =
      "  [" ++ (if done t then "x" else " ") ++ "] "
        ++ padLeft width (show (taskId t)) ++ "  " ++ title t
    nDone = length (filter done tasks)
    nLeft = length tasks - nDone
    summary =
      "  " ++ show nDone ++ " done, " ++ show nLeft
        ++ if d == today then " remaining" else " missed"
    body
      | null tasks = ["  no tasks"]
      | otherwise  = map line tasks ++ [summary]

padLeft :: Int -> String -> String
padLeft n s = replicate (n - length s) ' ' ++ s

-- Commands

listDay :: Day -> Day -> [Task] -> IO ()
listDay today d tasks =
  putStr (renderDay today d (sortOn taskId (filter ((== d) . day) tasks)))

listAll :: Day -> [Task] -> IO ()
listAll _ [] = putStrLn "no tasks yet"
listAll today tasks = putStr (concatMap render days)
  where
    days = groupBy ((==) `on` day) (sortOn (\t -> (Down (day t), taskId t)) tasks)
    render ts@(t : _) = renderDay today (day t) ts ++ "\n"
    render []         = ""

addTask :: Day -> String -> [Task] -> IO ()
addTask today name tasks = do
  let newId = 1 + maximum (0 : map taskId tasks)
      task = Task {taskId = newId, title = name, done = False, day = today}
  saveTasks (tasks ++ [task])
  putStrLn ("added " ++ show newId ++ ": " ++ name)

usage :: String
usage =
  unlines
    [ "usage:"
    , "  venato add <task>                        add a task for today"
    , "  venato list [yesterday | YYYY-MM-DD | all]  show tasks (default: today)"
    ]

main :: IO ()
main = do
  args <- getArgs
  today <- localDay . zonedTimeToLocalTime <$> getZonedTime
  tasks <- loadTasks
  case args of
    ("add" : ws)
      | not (all isSpace (unwords ws)) -> addTask today (unwords ws) tasks
    [] -> listDay today today tasks
    ["list"] -> listDay today today tasks
    ["list", "yesterday"] -> listDay today (addDays (-1) today) tasks
    ["list", "all"] -> listAll today tasks
    ["list", s]
      | Just d <- readMaybe s -> listDay today d tasks
    ["help"] -> putStr usage
    _ -> hPutStr stderr usage >> exitFailure
