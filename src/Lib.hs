{-# LANGUAGE NumericUnderscores #-}
module Lib
  ( runCli,
    match,
    runMatchers,
    Matcher (..),
    matcher,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (mapM_, void, when)
import qualified Data.Dates as Dates
import Data.Either (partitionEithers)
import Data.List (intercalate, isInfixOf)
import qualified Options.Applicative as Opt
import Options.Applicative ((<**>))
import qualified Options.Applicative.Help.Pretty as Doc
import qualified System.Console.ANSI as Terminal
import System.Exit (ExitCode (..), die, exitWith)
import qualified System.Process as Proc
import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>))
import qualified Text.Parsec.Char as ParsecChar
import Text.Parsec.Combinator (eof)
import qualified Text.Parsec.Expr as E
import qualified Text.Parsec.String as Parsec
import Text.Printf (printf)

-- Utils
isError :: ExitCode -> Bool
isError ExitSuccess = False
isError _ = True

time :: Dates.DateTime -> String
time d =
  (show yr) ++ "/"
    ++ (printf "%02d" mon)
    ++ "/"
    ++ (printf "%02d" day)
    ++ " "
    ++ (printf "%02d" hr)
    ++ ":"
    ++ (printf "%02d" min')
    ++ ":"
    ++ (printf "%02d" sec)
  where
    (yr, mon, day, hr, min', sec) = (Dates.year d, Dates.month d, Dates.day d, Dates.hour d, Dates.minute d, Dates.second d)

mapFirst :: (a -> c) -> (a, b) -> (c, b)
mapFirst f (a, b) = (f a, b)

-- Parsing
data Matcher
  = Pattern String
  | NonPattern String
  | And Matcher Matcher
  | Or Matcher Matcher
  deriving (Show, Eq)

doubleQuotes :: Parsec.Parser String
doubleQuotes =
  Parsec.between (symbol "\"") (symbol "\"")
    $ Parsec.many1
    $ ParsecChar.noneOf "\n\""

singleQuotes :: Parsec.Parser String
singleQuotes =
  Parsec.between (symbol "'") (symbol "'")
    $ Parsec.many1
    $ ParsecChar.noneOf "\n'"

term' :: Parsec.Parser String
term' = Parsec.many1 $ ParsecChar.noneOf "\n\t !|&"

term :: Parsec.Parser Matcher
term = Pattern <$> lexeme (doubleQuotes <|> singleQuotes <|> term')

matcher :: Parsec.Parser Matcher
matcher =
  whitespace
    *> ( E.buildExpressionParser
           [ [prefix "!" negate'],
             [binary "&" And E.AssocLeft],
             [binary "|" Or E.AssocLeft]
           ]
           term
       )
    -- there should only be one expression, content should end after
    <* whitespace
    <* eof
  where
    prefix c op = E.Prefix (op <$ symbol c)
    binary c op = E.Infix (op <$ symbol c)
    -- This is intentionally non-exhaustive
    negate' (Pattern str) = NonPattern str
    negate' _ = error "Unexpected pattern provided to '!'"

whitespace :: Parsec.Parser ()
whitespace = void $ Parsec.many $ ParsecChar.oneOf " \t"

lexeme :: Parsec.Parser a -> Parsec.Parser a
lexeme p = p <* whitespace

symbol :: String -> Parsec.Parser String
symbol s = lexeme $ ParsecChar.string s

-- Eval
type Line = String

match :: Matcher -> Line -> Bool
match _ "" = False
match (Pattern pattern) line = pattern `isInfixOf` line
match (NonPattern pattern) line = not (pattern `isInfixOf` line)
match (And a b) line = match a line && match b line
match (Or a b) line = match a line || match b line

runMatchers :: [Matcher] -> [Line] -> Bool
runMatchers [] _ = True
runMatchers _ [] = False
runMatchers [m] lns = any (match m) lns
runMatchers ms lns = or (match <$> ms <*> lns)

-- Checking & output

-- Reference: https://github.com/feuerbach/ansi-terminal/blob/d11ceb19fc8e85b4eea281c1e239156bf7a2e9cd/app/Example.hs#L33-L40
resetScreen :: IO ()
resetScreen =
  Terminal.setSGR [Terminal.Reset]
    >> Terminal.clearScreen
    >> Terminal.setCursorPosition 0 0

type Stdout = String

type Stderr = String

run :: Cmd -> IO (ExitCode, Stdout, Stderr)
run cmd' = Proc.readCreateProcessWithExitCode (Proc.shell cmd') ""

emptyLine :: IO ()
emptyLine = putStrLn ""

check :: Env -> IO ()
check env = do
  (exitCode, stdout, _) <- run $ envCmd env
  when (not (envContinueOnError env) && isError exitCode) $ exitWith exitCode
  let content = lines stdout
  output ("$ " ++ envCmd env) content
  if runMatchers (envMatchers env) content
    then do
      -- TODO print status report & last output
      emptyLine
      putStrLn "Found match, exiting ..."
    else do
      let second = 1_000_000
      threadDelay $ envInterval env * second
      check env

-- TODO: header + date might be longer than width
mkHeader :: String -> Int -> String -> String
mkHeader header width date = header ++ space ++ "[" ++ date ++ "] "
  where
    -- -3 for the brackets and space
    n = width - 3 - (length header + length date)
    space = take n $ repeat ' '

-- TODO: takes one tick sometimes to correct output after end
-- TODO: disable scrolling, it messes up output real bad
-- TODO: disable cursor? also messes output
output :: String -> [String] -> IO ()
output header content = do
  now <- Dates.getCurrentDateTime
  mSize <- Terminal.getTerminalSize
  -- subtract 2 -> height minus header and empty line
  -- height default: content lines count
  -- width  default: 80 because reasons /srug
  let (height, width) = maybe (length content, 80) (mapFirst $ subtract 2) mSize
  Terminal.setCursorPosition 0 0
  putStrLn $ mkHeader header width $ time now
  emptyLine
  mapM_ putStrLnAndClearRest $ take height content
  Terminal.clearFromCursorToScreenEnd

putStrLnAndClearRest :: String -> IO ()
putStrLnAndClearRest line = do
  putStr line
  Terminal.clearFromCursorToLineEnd
  putStr "\n"

-- CLI Args
type Cmd = String

data Conf
  = Conf
      { confCmd :: Cmd,
        confPatterns :: [String],
        confInterval :: Int,
        confContinueOnError :: Bool
      }
  deriving (Show)

data Env
  = Env
      { envCmd :: Cmd,
        envMatchers :: [Matcher],
        envInterval :: Int,
        envContinueOnError :: Bool
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

parseArgs :: Opt.ParserInfo a -> IO a
parseArgs =
  Opt.customExecParser $
    Opt.prefs (Opt.idm <> Opt.showHelpOnError <> Opt.showHelpOnEmpty)

confToEnv :: Conf -> [Matcher] -> Env
confToEnv conf matchers =
  Env
    { envCmd = confCmd conf,
      envMatchers = matchers,
      envInterval = confInterval conf,
      envContinueOnError = confContinueOnError conf
    }

accEithers :: Show a => [Either a b] -> Either String [b]
accEithers eithers = case partitionEithers eithers of
  ([], bs) -> Right bs
  (as, _) -> Left $ unlines $ fmap show as

runCli :: IO ()
runCli = do
  conf <- parseArgs confP
  case accEithers $ fmap (Parsec.parse matcher "") $ confPatterns conf of
    Right matchers -> do
      resetScreen
      check $ confToEnv conf matchers
    Left errors -> die errors
