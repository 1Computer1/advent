{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RecordWildCards  #-}

module Advent.Year2020.Day04 where

import Prelude hiding ((>>=), (>>), fail)
import Data.Char
import Data.List.Split
import Data.Maybe
import Text.Read

data Passport = Passport
    { byr, iyr, eyr, hgt, hcl, ecl, pid, cid :: Maybe String
    }

solutionA :: String -> Int
solutionA input = length $ filter validPassport ps
    where ps = parseInput input

parseInput :: String -> [Passport]
parseInput xs = map (parsePassport . map (two . splitOn ":") . splitOneOf " \n") (splitOn "\n\n" xs)
    where
        two (x:y:_) = (x, y)
        parsePassport ls = Passport
            { byr = lookup "byr" ls
            , iyr = lookup "iyr" ls
            , eyr = lookup "eyr" ls
            , hgt = lookup "hgt" ls
            , hcl = lookup "hcl" ls
            , ecl = lookup "ecl" ls
            , pid = lookup "pid" ls
            , cid = lookup "cid" ls
            }

validPassport :: Passport -> Bool
validPassport (Passport {..}) = all isJust [byr, iyr, eyr, hgt, hcl, ecl, pid]

solutionB :: String -> Int
solutionB input = length $ filter validPassport' ps
    where ps = parseInput input

validPassport' :: Passport -> Bool
validPassport' (Passport {..}) = do
    Just byr' <- byr
    length byr' == 4
    case readMaybe byr' :: Maybe Int of
        Just n -> 1920 <= n && n <= 2002
        Nothing -> False

    Just iyr' <- iyr
    length iyr' == 4
    case readMaybe iyr' :: Maybe Int of
        Just n -> 2010 <= n && n <= 2020
        Nothing -> False

    Just eyr' <- eyr
    length eyr' == 4
    case readMaybe eyr' :: Maybe Int of
        Just n -> 2020 <= n && n <= 2030
        Nothing -> False

    Just hgt' <- hgt
    let (ns, us) = span isDigit hgt'
    case (readMaybe ns :: Maybe Int, us) of
        (Just n, "cm") -> 150 <= n && n <= 193
        (Just n, "in") -> 59 <= n && n <= 76
        _ -> False

    Just ('#':hcl') <- hcl
    length hcl' == 6
    all (`elem` ['0'..'9'] <> ['a'..'f']) hcl'

    Just ecl' <- ecl
    ecl' `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

    Just pid' <- pid
    length pid' == 9
    Just _ <- readMaybe pid' :: Maybe Int

    True
    where
        (>>=) = flip ($)
        (>>) = (&&)
        fail _ = False
