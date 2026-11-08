module Oppentimer.Audio where

import Foreign.C (CString)
import Foreign.C.String (withCAString)
import Foreign.C.Types (CInt (..))

foreign import ccall unsafe "tPlayAudio"
        tPlayAudio :: CString -> IO CInt

haskellPlayAudio :: String -> IO ()
haskellPlayAudio file_path = do
        withCAString file_path tPlayAudio >> pure ()
