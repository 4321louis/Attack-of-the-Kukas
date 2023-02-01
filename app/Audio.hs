

module Audio where

import System.Environment
import System.FilePath
import qualified Data.ByteString as SB
import Control.Concurrent
import Control.Monad

import Sound.ProteaAudio

waitPlayback = do
    n <- soundActiveAll
    when  (n > 0) $ do
        threadDelay oneSec
        waitPlayback

oneSec :: Int
oneSec = 1000000 -- micro seconds

tempAudioMain = do
    let filename = "./src/menuLoop.wav"

    result <- initAudio 64 44100 1024 -- max channels, mixing frequency, mixing buffer size
    unless result $ fail "failed to initialize the audio system"

    -- (A) load sample from file
    sampleA <- sampleFromFile filename 1.0 -- volume

    -- start two sound tracks with shared sample data
    sndTrkA <- soundPlay sampleA 1 0 0 1 -- left volume, right volume, time difference between left and right, pitch factor for playback
    threadDelay oneSec -- wait 1 sec
    sndTrkB <- soundPlay sampleA 0 1 0 1 -- left volume, right volume, time difference between left and right, pitch factor for playback
    soundActive sndTrkB >>= print
    -- play 3 sec
    threadDelay $ 3 * oneSec
    soundStop sndTrkB
    soundActive sndTrkB >>= print
    -- wait sndTrkA to finish
    waitPlayback

    -- (B) load from memory buffer
    buffer <- SB.readFile filename
    sampleB <- case takeExtension filename of
        ".ogg" -> sampleFromMemoryOgg buffer 1.0
        ".wav" -> sampleFromMemoryWav buffer 1.0

    soundPlay sampleB 1 1 0 1 -- left volume, right volume, time difference between left and right, pitch factor for playback
    waitPlayback

    sampleDestroy sampleB
    soundPlay sampleB 1 1 0 1 -- we have invalidated the handle; nothing should happen now
    waitPlayback

    finishAudio