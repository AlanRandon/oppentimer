{-# LANGUAGE ForeignFunctionInterface #-}


import Foreign.C.Types (CInt(..))
import Foreign.C (CString)
import Foreign.C.String(withCAString)


foreign import ccall safe "play_audio"
    c_play_audio :: CString -> IO CInt

haskell_play_audio :: String -> IO CInt
haskell_play_audio file_path = do
    withCAString file_path c_play_audio

main :: IO ()
main = do
    returnInt <- haskell_play_audio "foxy-scream-fnaf-1.mp3"
    print returnInt