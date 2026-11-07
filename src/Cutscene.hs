{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Cutscene (cutsceneMain) where



import oppentimerMain
import Apecs
import Apecs.Gloss
import Data.Maybe (fromMaybe)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import System.Exit (exitSuccess)

