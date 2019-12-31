module Lib
  ( runCli,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (mapM_, when)
import Data.List (intercalate, isInfixOf)
import qualified Options.Applicative as Opt
import Options.Applicative ((<**>))
import qualified Options.Applicative.Help.Pretty as Doc
import qualified System.Console.ANSI as Terminal
import System.Exit (ExitCode (..), exitWith)
import qualified System.Process as Proc

type Stdout = String

type Stderr = String

type Cmd = String

type Pattern = String

second :: Int
second = 1000000

run :: Cmd -> IO (ExitCode, Stdout, Stderr)
run cmd' = Proc.readCreateProcessWithExitCode (Proc.shell cmd') ""

isError :: ExitCode -> Bool
isError ExitSuccess = False
isError _ = True

-- Reference: https://github.com/feuerbach/ansi-terminal/blob/d11ceb19fc8e85b4eea281c1e239156bf7a2e9cd/app/Example.hs#L33-L40
resetScreen :: IO ()
resetScreen =
  Terminal.setSGR [Terminal.Reset]
    >> Terminal.clearScreen
    >> Terminal.setCursorPosition 0 0

data Conf
  = Conf
      { cmd :: Cmd,
        patterns :: [Pattern],
        interval :: Int,
        continueOnError :: Bool
      }
  deriving (Show)

confP :: Opt.ParserInfo Conf
confP =
  Opt.info (p <**> Opt.helper) (Opt.fullDesc <> Opt.headerDoc headerText)
  where
    -- TODO: improve help messages
    p :: Opt.Parser Conf
    p =
      Conf <$> Opt.argument Opt.str (Opt.metavar "CMD" <> Opt.help "Command to run")
        <*> Opt.many (Opt.argument Opt.str (Opt.metavar "PATTERN" <> Opt.help "Patterns to match against CMD's output"))
        <*> Opt.option Opt.auto (Opt.value 2 <> Opt.long "interval" <> Opt.short 'i' <> Opt.metavar "SECONDS" <> Opt.help "The interval (in seconds) to run CMD")
        <*> Opt.switch (Opt.long "continue-on-error" <> Opt.short 'e' <> Opt.help "Keep trying if CMD exits with non zero result")
    headerText :: Maybe Doc.Doc
    headerText =
      Just $ Doc.string $
        intercalate
          "\n"
          [ "till v0.0.1\n",
            "Execute a command until it's output matches certain conditions.\n",
            "Project's Home Page: https://github.com/gilchristian/till"
          ]

type Line = String

match :: Conf -> Line -> Bool
match conf line = all (`isInfixOf` line) $ patterns conf

check :: Conf -> IO ()
check conf = do
  (exitCode, stdout, _) <- run $ cmd conf
  when (not (continueOnError conf) && isError exitCode) $ exitWith exitCode
  let result = filter (match conf) $ lines stdout
  output ("$ " ++ cmd conf) stdout
  if null result
    then do
      threadDelay $ interval conf * second
      check conf
    else do
      Terminal.setCursorPosition 0 0
      Terminal.clearFromCursorToScreenEnd
      -- TODO print status report
      putStrLn "Found match, exiting ..."

output :: String -> String -> IO ()
output header content = do
  -- TODO: remove extra output
  mSize <- Terminal.getTerminalSize
  let lns = lines content
  let height = case mSize of
        Just (h, _) -> h - 2 -- minus header and empty line
        Nothing -> length lns
  Terminal.setCursorPosition 0 0

  putStrLn header
  putStrLn ""

  mapM_ putStrLnAndClearRest $ take height lns

  -- Double check if this works
  Terminal.clearFromCursorToScreenEnd

putStrLnAndClearRest :: String -> IO ()
putStrLnAndClearRest line = do
  putStr line
  Terminal.clearFromCursorToLineEnd
  putStr "\n"

parse :: Opt.ParserInfo a -> IO a
parse =
  Opt.customExecParser $
    Opt.prefs (Opt.idm <> Opt.showHelpOnError <> Opt.showHelpOnEmpty)

runCli :: IO ()
runCli = do
  conf <- parse confP
  resetScreen
  check conf
