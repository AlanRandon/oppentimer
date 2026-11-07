{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Oppentimer (oppentimerMain) where

import Apecs
import Apecs.Gloss
import System.Exit (exitSuccess)

-------------------------------------------
-- Components
-------------------------------------------

newtype Position = Pos (Float, Float)
instance Component Position where type Storage Position = Map Position

newtype Velocity = Vel (Float, Float)
instance Component Velocity where type Storage Velocity = Map Velocity

-------------------------------------------
-- Model
-------------------------------------------

makeWorld "Model" [''Camera, ''Position, ''Velocity]

type System' a = System Model a

-------------------------------------------
-- View
-------------------------------------------

view :: System' Picture
view =
        foldDraw -- cram everything into one picture
                ( \(Pos (x, y)) ->
                        translate x y $ color white $ (circle 10)
                )

-------------------------------------------
-- Controller
-------------------------------------------

inputController :: Event -> System' ()
inputController event = case event of
        EventKey (SpecialKey KeyEsc) Down _ _ ->
                liftIO exitSuccess
        EventKey (Char 'q') Down _ _ -> liftIO exitSuccess
        EventKey (SpecialKey KeySpace) Down _ _ -> cmap (\(Pos _) -> (Pos (0, 0), Vel (0, 0)))
        EventKey (Char 'h') Down _ _ -> cmap (\(Vel (vX, vY)) -> Vel (vX - 20, vY))
        EventKey (Char 'j') Down _ _ -> cmap (\(Vel (vX, vY)) -> Vel (vX, vY - 20))
        EventKey (Char 'k') Down _ _ -> cmap (\(Vel (vX, vY)) -> Vel (vX, vY + 20))
        EventKey (Char 'l') Down _ _ -> cmap (\(Vel (vX, vY)) -> Vel (vX + 20, vY))
        _ -> pure ()

stepPos :: Float -> System' ()
stepPos dt =
        cmap
                ( \(Pos (x, y), Vel (vX, vY)) ->
                        -- search for all entities with pos and vel attributes, update
                        Pos (x + vX * dt, y + vY * dt)
                )

-------------------------------------------
-- Spawn entities
-------------------------------------------

createEntities :: System' ()
createEntities = do
        newEntity_ (Pos (0, 0), Vel (0, 0))

-------------------------------------------
-- Run game
-------------------------------------------

game :: System' ()
game = do
        set global (Camera 0 5)
        createEntities
        play
                display
                black
                60 -- FPS
                view
                inputController
                ( \dt -> do
                        stepPos dt
                )
    where
        display = InWindow "Basic" (1024, 1024) (10, 10)

oppentimerMain :: IO ()
oppentimerMain = do
        w <- initModel
        runWith w game
