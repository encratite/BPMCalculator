module Main(
  main
  ) where

import Control.Monad
import qualified Data.Sequence as DS
import Data.Time.Clock

type Beats = DS.Seq UTCTime

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
      difference = (realToFrac (diffUTCTime newTime oldTime) :: Double) * (10.0 ** (- 12))
      currentLength = DS.length newBeats
      beatCount = currentLength - 1
      beatsPerMinute = ((fromIntegral beatCount :: Double) / difference) * 60
      gotEnough = currentLength >= minimumBeatCount
      beatsRequired = minimumBeatCount - currentLength
  putStrLn $ if gotEnough
             then show beatsPerMinute
             else "Need " ++ show beatsRequired ++ " more"
  calculateBPM newBeats

main :: IO ()
main = calculateBPM DS.empty