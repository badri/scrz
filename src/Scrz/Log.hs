module Scrz.Log (logger, tabWriter) where

import Data.List           (intercalate, transpose)
import Data.Time.LocalTime (ZonedTime, getZonedTime)
import Data.Time.Format    (formatTime)

import Text.Printf         (printf)
import System.Locale       (defaultTimeLocale)


cleanCalendar :: ZonedTime -> String
cleanCalendar = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

logger :: String -> IO ()
logger msg = do
    zt <- getZonedTime
    printf "[%s] %s\n" (cleanCalendar zt) msg


-- | Print the rows so that columns are properly aligned below each other.
tabWriter :: [ [ String ] ] -> IO ()
tabWriter d = mapM_ writeRow d

  where
    lengths = map maximum $ transpose $ (map (map length) d)

    writeRow :: [ String ] -> IO ()
    writeRow row = do
        let fields = map expandField (zip lengths row)
        putStrLn $ intercalate "   " fields

    expandField :: (Int, String) -> String
    expandField (maxLength, field) = fld ++ trail
        where fld   = take maxLength field
              trail = (replicate (maxLength - (length fld)) ' ')
