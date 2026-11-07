-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TypeFamilies #-}

-- module Cutscene (cutsceneMain) where



import Oppentimer
import Apecs
import Apecs.Gloss
import Data.Maybe (fromMaybe)
import Graphics.Gloss.Juicy (loadJuicyPNG)
import System.Exit (exitSuccess)

newtype Background = Background Picture
instance Component Background where type Storage Background = Map Background

makeWorld
        "Model"
        [ ''Camera
        , ''Background
        ]

type System' b c = System c


cutsceneMain :: IO()

cutsceneMain = do print "."