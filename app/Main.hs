module Main (main) where

import Control.Exception (evaluate)
import Data.Char (isSpace)
import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Ord (Down (..))
import Data.Time (Day, addDays, getZonedTime, localDay, zonedTimeToLocalTime)
import System.Directory (doesFileExist, getHomeDirectory, renameFile)
import System.Environment (getArgs, lookupEnv)
import System.Exit (die, exitFailure)
import System.FilePath ((</>))
import System.IO (Handle, hIsTerminalDevice, hPutStr, stderr, stdout)
import Text.Read (readMaybe)

data Task = Task
  { taskId :: Int
  , title  :: String
  , done   :: Bool
  , day    :: Day
  }
  deriving (Show, Read)

-- Colors: raw ANSI codes, only when writing to a terminal and NO_COLOR is unset

useColor :: Handle -> IO Bool
useColor h = do
  tty <- hIsTerminalDevice h
  noColor <- lookupEnv "NO_COLOR"
  pure (tty && maybe True null noColor)

paint :: Bool -> String -> String -> String
paint False _ s    = s
paint True code s  = "\ESC[" ++ code ++ "m" ++ s ++ "\ESC[0m"

bold, dim, red, green, yellow, cyan :: String
bold   = "1"
dim    = "2"
red    = "31"
green  = "32"
yellow = "33"
cyan   = "36"

abort :: String -> IO a
abort msg = do
  c <- useColor stderr
  die (paint c red "venato:" ++ " " ++ msg)

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
        Nothing    -> abort ("could not parse " ++ path)

saveTasks :: [Task] -> IO ()
saveTasks tasks = do
  path <- storePath
  let tmp = path ++ ".tmp"
  writeFile tmp (unlines (map show tasks))
  renameFile tmp path

-- Rendering

renderDay :: Bool -> Day -> Day -> [Task] -> String
renderDay c today d tasks = unlines (header : body)
  where
    isToday = d == today
    header =
      paint c bold (show d) ++ " "
        ++ if isToday then paint c cyan "(today)" else paint c dim "(read-only)"
    width = maximum (0 : map (length . show . taskId) tasks)
    line t =
      "  " ++ box t ++ " "
        ++ paint c dim (padLeft width (show (taskId t))) ++ "  "
        ++ (if done t then paint c dim (title t) else title t)
    box t
      | done t    = paint c green "[x]"
      | isToday   = paint c yellow "[ ]"
      | otherwise = paint c red "[ ]"
    nDone = length (filter done tasks)
    nLeft = length tasks - nDone
    summary =
      "  " ++ paint c green (show nDone ++ " done") ++ paint c dim ", "
        ++ if isToday
          then paint c yellow (show nLeft ++ " remaining")
          else paint c red (show nLeft ++ " missed")
    body
      | null tasks = [paint c dim "  no tasks"]
      | otherwise  = map line tasks ++ [summary]

padLeft :: Int -> String -> String
padLeft n s = replicate (n - length s) ' ' ++ s

-- Commands

listDay :: Bool -> Day -> Day -> [Task] -> IO ()
listDay c today d tasks =
  putStr (renderDay c today d (sortOn taskId (filter ((== d) . day) tasks)))

listAll :: Bool -> Day -> [Task] -> IO ()
listAll c _ [] = putStrLn (paint c dim "no tasks yet")
listAll c today tasks = putStr (concatMap render days)
  where
    days = groupBy ((==) `on` day) (sortOn (\t -> (Down (day t), taskId t)) tasks)
    render ts@(t : _) = renderDay c today (day t) ts ++ "\n"
    render []         = ""

report :: Bool -> String -> String -> Task -> IO ()
report c color verb t =
  putStrLn (paint c color verb ++ " " ++ paint c dim (show (taskId t)) ++ "  " ++ title t)

addTask :: Bool -> Day -> String -> [Task] -> IO ()
addTask c today name tasks = do
  let newId = 1 + maximum (0 : map taskId tasks)
      task = Task {taskId = newId, title = name, done = False, day = today}
  saveTasks (tasks ++ [task])
  report c green "added" task

-- Past days are read-only, so only today's tasks can be changed.
withTodayTask :: Day -> String -> [Task] -> (Task -> IO ()) -> IO ()
withTodayTask today s tasks k =
  case readMaybe s of
    Nothing -> abort ("not a task id: " ++ s)
    Just i -> case filter ((== i) . taskId) tasks of
      [] -> abort ("no task with id " ++ show i)
      (t : _)
        | day t /= today ->
            abort ("task " ++ show i ++ " is from " ++ show (day t) ++ " and is read-only")
        | otherwise -> k t

completeTask :: Bool -> Day -> String -> [Task] -> IO ()
completeTask c today s tasks = withTodayTask today s tasks $ \t ->
  if done t
    then report c dim "already done" t
    else do
      saveTasks [if taskId u == taskId t then u {done = True} else u | u <- tasks]
      report c green "completed" t

removeTask :: Bool -> Day -> String -> [Task] -> IO ()
removeTask c today s tasks = withTodayTask today s tasks $ \t -> do
  saveTasks (filter ((/= taskId t) . taskId) tasks)
  report c yellow "removed" t

usage :: String
usage =
  unlines
    [ "usage:"
    , "  venato add <task>                           add a task for today"
    , "  venato list [yesterday | YYYY-MM-DD | all]  show tasks (default: today)"
    , "  venato done <id>                            mark one of today's tasks done"
    , "  venato rm <id>                              remove one of today's tasks"
    ]

main :: IO ()
main = do
  args <- getArgs
  c <- useColor stdout
  today <- localDay . zonedTimeToLocalTime <$> getZonedTime
  tasks <- loadTasks
  case args of
    ("add" : ws)
      | not (all isSpace (unwords ws)) -> addTask c today (unwords ws) tasks
    [] -> listDay c today today tasks
    ["list"] -> listDay c today today tasks
    ["list", "yesterday"] -> listDay c today (addDays (-1) today) tasks
    ["list", "all"] -> listAll c today tasks
    ["list", s]
      | Just d <- readMaybe s -> listDay c today d tasks
    ["done", s] -> completeTask c today s tasks
    ["rm", s] -> removeTask c today s tasks
    ["help"] -> putStr usage
    _ -> hPutStr stderr usage >> exitFailure
