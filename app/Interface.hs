module Interface
    ( Options(..)
    , getOptions
    ) where

import           Data.Char
import           Options.Applicative
import           Solution
import qualified Text.Parsec as P
import           Types

data Options = Options
    { year :: Year
    , problems :: [(Day, Part)]
    }

type Parsec = P.Parsec String ()

getOptions :: IO Options
getOptions = execParser options

options :: ParserInfo Options
options = info (helper <*> optionsP)
    (fullDesc <> progDesc "Run Advent of Code solutions")

optionsP :: Parser Options
optionsP = Options
    <$> argument auto (metavar "YEAR")
    <*> some (argument (maybeReader (hush . P.parse argP "")) (metavar "PROBLEMS..."))

argP :: Parsec (Day, Part)
argP = (,) <$> (read <$> P.many1 P.digit) <*> (partFromChar <$> P.oneOf "abAB")

hush :: Either a b -> Maybe b
hush = \case
    Left _ -> Nothing
    Right x -> Just x
