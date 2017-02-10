module Main where

import Data.Time
import Data.Time.Format
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Control.Monad

data LogEntry = LogEntry { date :: UTCTime
                         , rqmethod :: T.Text
                         , url :: T.Text
                         , query :: T.Text
                         , useragent :: T.Text
                         , status :: Int
                         , bytesout :: Int
                         , bytesin :: Int 
                         } deriving (Show)

parseDateString :: T.Text -> UTCTime
parseDateString s = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S" (T.unpack s)

parseInt :: T.Text -> Int
parseInt t = read $ (T.unpack t) :: Int

splitLogLine :: T.Text -> [T.Text]
splitLogLine = T.splitOn (T.pack " ")

toLogEntry :: [T.Text] -> LogEntry
toLogEntry t =
    let ds = T.concat [(t!!0),(T.pack " "),(t!!1)]
    in LogEntry { date=(parseDateString ds)
                , rqmethod=(t!!3)
                , url=(t!!4)
                , query=(t!!5)
                , useragent=(t!!8)
                , status=(parseInt (t!!11))
                , bytesout=(parseInt (t!!12))
                , bytesin=(parseInt (t!!13))
                }

isValidLine :: T.Text -> Bool
isValidLine = T.isInfixOf (T.pack "/us ")

forDisplay :: LogEntry -> (UTCTime, T.Text, Int)
forDisplay e = (date e, T.concat [rqmethod e, (T.pack " "), url e, (T.pack " "), query e], status e) 

main :: IO ()
main = do
    ls <- fmap T.lines (T.readFile "logs/sml.log")
    let filteredLines = filter isValidLine ls
        arrays = map splitLogLine filteredLines
        entries = map toLogEntry arrays
    --let output = T.unlines entries
    --T.writeFile "E:/Src/haskell/test.log" output
    mapM_ print (map forDisplay entries)