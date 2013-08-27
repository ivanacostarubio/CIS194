-- CIS 194 Homework 2

module Log where

import Control.Applicative
import Data.List   
import Data.Char

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

getNumber :: String -> TimeStamp
getNumber (x) = read x :: Int


getComment :: [String] -> String
getComment (x) = unwords x

parseMessage :: String -> LogMessage
parseMessage (x) = parseMessageWords(words(x))

parseMessageWords :: [String]-> LogMessage
parseMessageWords ("I":x:xs) = LogMessage Info (getNumber(x)) (getComment(xs))
parseMessageWords ("W":x:xs) = LogMessage Warning (getNumber(x)) (getComment(xs))
parseMessageWords ("E":y:x:xs) = LogMessage (Error (getNumber(y))) (getNumber(x)) (getComment(xs))
parseMessageWords (x) = Unknown (getComment(x))

parseLogList :: [String] -> [LogMessage]
parseLogList (x:xs) = parseMessage(x) : parseLogList(xs)
parseLogList [] = []

parseLogFile :: String -> [LogMessage]
parseLogFile (x) =  parseLogList(lines x)



-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
testParse parse n file = take n . parse <$> readFile file

-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
testWhatWentWrong :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO [String]
testWhatWentWrong parse whatWentWrong file
  = whatWentWrong . parse <$> readFile file
