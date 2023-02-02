

module Audio where

import System.Environment
import System.FilePath
import qualified Data.ByteString as SB
import Control.Concurrent
import Control.Monad

import Sound.ProteaAudio
import Apecs


gameLoop, pew :: IO Sample
gameLoopIntro = sampleFromFile "./assets/Audio/Music/gameLoopIntro.ogg" 0.3
gameLoop = sampleFromFile "./assets/Audio/Music/gameLoop.ogg" 0.3
pew = sampleFromFile "./assets/Audio/Effects/PEW.wav" 1.0

playSoundEffect :: Sample -> System w Sound
playSoundEffect = liftIO . (\s -> soundPlay s 1 1 0 1)
playIOSoundEffect :: IO Sample -> System w Sound
playIOSoundEffect iosample = do
    sample <- liftIO iosample
    liftIO $ soundPlay sample 1 1 0 1

-- waitPlayback :: IO ()
-- waitPlayback = do
--     n <- soundActiveAll
--     when  (n > 0) $ do
--         threadDelay oneSec
--         waitPlayback

waitSound :: Sound -> IO ()
waitSound s = do
    playing <- soundActive s
    when  (playing) $ do
        threadDelay (div oneSec 100)
        waitSound s

oneSec :: Int
oneSec = 1000000 -- micro seconds

tempAudioMain :: IO ()
tempAudioMain = do
    result <- initAudio 128 44100 1024 -- max channels, mixing frequency, mixing buffer size
    unless result $ fail "failed to initialize the audio system"

    void $ forkIO $ do 
        gameLoop <- gameLoop
        gameLoopIntro <- gameLoopIntro
        intro <- soundPlay gameLoopIntro 1 1 0 1
        waitSound intro
        _musicTrack <- soundLoop gameLoop 1 1 0 1
        return ()
    