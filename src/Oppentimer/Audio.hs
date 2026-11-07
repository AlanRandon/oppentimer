module Oppentimer.Audio where

import Foreign.C (CString)
import Foreign.C.String (withCAString)
import Foreign.C.Types (CInt (..))

foreign import ccall unsafe "play_audio"
        c_play_audio :: CString -> IO CInt

haskellPlayAudio :: String -> IO CInt
haskellPlayAudio file_path = do
        withCAString file_path c_play_audio
