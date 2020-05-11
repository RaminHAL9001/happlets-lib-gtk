module Happlets.Provider.ALSA where

import           Happlets.View.Audio

import           Control.Concurrent

import           Foreign.Storable

import qualified Sound.ALSA.PCM     as Linux
import qualified Sound.Frame.Stereo as Stereo

----------------------------------------------------------------------------------------------------

newtype LinuxAudioOut sample fmt = LinuxAudioOut (MVar (FrameCounter -> PCM sample))
type StereoPulseCode = Stereo.T PulseCode

-- | This is the frame rate used by default when installing an animation event handler. This value
-- can be configured by modifying the 'Happlets.Config.animationFrameRate'.
gtkAnimationFrameRate :: Double
gtkAnimationFrameRate = 30.0

stereoFormat :: (LeftPulseCode, RightPulseCode) -> StereoPulseCode
stereoFormat = uncurry Stereo.cons

-- TODO: determine a more platform-agnostic way of obtaining the default device ID.
audioDeviceID :: IO String
audioDeviceID = return "pulse"

minAudioBufferSize :: Int
minAudioBufferSize = ceiling $ gtkAnimationFrameRate / realToFrac audioSampleRate

makeAudioSource
  :: Storable fmt
  => (sample -> fmt) -> SampleCount Int -> MVar (FrameCounter -> PCM sample)
  -> Linux.SoundSource (LinuxAudioOut sample) fmt
makeAudioSource format _bufSize mvar =
  Linux.SoundSource
  { Linux.soundSourceOpen  = return $ LinuxAudioOut mvar --newEmptyMVar
  , Linux.soundSourceClose = \ (LinuxAudioOut _mvar) -> return () --void $ takeMVar mvar
  , Linux.soundSourceStart = \ (LinuxAudioOut _mvar) -> return () --newSt >>= putMVar mvar
  , Linux.soundSourceStop  = \ (LinuxAudioOut _mvar) -> return () --void $ takeMVar mvar
  , Linux.soundSourceRead  = \ (LinuxAudioOut mvar) ptr bufSize -> do
      let writer t i pcm = if i >= bufSize then return (pcm, t) else do
            sample <- runPCM $ pcm t
            pokeElemOff ptr i $ format sample
            ((writer $! t + 1) $! i + 1) pcm
      let loop t = modifyMVar mvar (writer t 0) >>= loop
      loop 0
  }   

type MakeAudioSource sample fmt
  =  SampleCount Int
  -> MVar (FrameCounter -> PCM sample)
  -> Linux.SoundSource (LinuxAudioOut sample) fmt

makeStereoSource :: MakeAudioSource (LeftPulseCode, RightPulseCode) StereoPulseCode
makeStereoSource = makeAudioSource stereoFormat

makeMonoSource :: MakeAudioSource PulseCode PulseCode
makeMonoSource = makeAudioSource id

data AudioPlaybackThread
  = MonoPlaybackThread
      !(Maybe ThreadId) !(MVar (FrameCounter -> PCM PulseCode))
  | StereoPlaybackThread
      !(Maybe ThreadId) !(MVar (FrameCounter -> PCM (LeftPulseCode, RightPulseCode)))

makeAudioFormat :: Storable fmt => Linux.SoundFmt fmt
makeAudioFormat = Linux.SoundFmt (round audioSampleRate)

startPlaybackThread :: BufferSizeRequest -> PCMGenerator -> IO AudioPlaybackThread
startPlaybackThread reqSize gen = do
  devID <- audioDeviceID
  let bufSize = max minAudioBufferSize $ ceiling $ reqSize * realToFrac audioSampleRate
  let sink :: Linux.SampleFmt fmt => Linux.SoundFmt fmt -> Linux.SoundSink Linux.Pcm fmt
      sink = Linux.alsaSoundSink devID
  let make :: Linux.SampleFmt fmt
        => (Maybe ThreadId -> MVar (FrameCounter -> PCM sample) -> AudioPlaybackThread)
        -> MakeAudioSource sample fmt
        -> Linux.SoundFmt fmt
        -> Int
        -> (FrameCounter -> PCM sample)
        -> IO AudioPlaybackThread
      make constr src linFmt chans gen = do
        mvar <- newMVar gen
        fmap (flip constr mvar . Just) $ forkOS $
          Linux.copySound (src bufSize mvar) (sink linFmt) $ chans * bufSize
  case gen of
    PCMGenerateStereo gen -> make StereoPlaybackThread makeStereoSource makeAudioFormat 2 gen
    PCMGenerateMono   gen -> make MonoPlaybackThread   makeMonoSource   makeAudioFormat 1 gen
