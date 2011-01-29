module Main(
  main
  ) where

import Control.Monad
import qualified Data.Sequence as DS
import Data.Time.Clock

type Beats = DS.Seq UTCTime

-- | Run a loop which waits for lines from stdin.
-- It then calculates the BPM based on the delay between them.
calculateBPM :: Beats -> IO ()
calculateBPM beats = do
  let minimumBeatCount = 5
      maximumBeatCount = 15
  void $ getLine
  newTime <- getCurrentTime
  let extendedBeats = beats DS.|> newTime
      tooLong = DS.length extendedBeats > maximumBeatCount
      newBeats = (if tooLong then DS.drop 1 else id) extendedBeats
      oldTime DS.:< _ = DS.viewl newBeats
      -- difference between the oldest and the newest timestamp stored, in seconds
      -- ensure that it is never zero by establishing a minimum of one millisecond
      difference = max (realToFrac (diffUTCTime newTime oldTime) :: Double) (10.0 ** (-3))
      currentLength = DS.length newBeats
      beatCount = currentLength - 1
      beatsPerMinute = round (((fromIntegral beatCount :: Double) / difference) * 60) :: Int
      gotEnough = currentLength >= minimumBeatCount
      beatsRequired = minimumBeatCount - currentLength
  putStrLn $ if gotEnough
             then show beatsPerMinute
             else "Need " ++ show beatsRequired ++ " more"
  calculateBPM newBeats

main :: IO ()
main = do
  putStrLn "Hit enter to start counting"
  calculateBPM DS.empty