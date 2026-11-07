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

data Direction = DirectionLeft | DirectionRight | DirectionUp | DirectionDown | DirectionStationary
instance Component Direction where type Storage Direction = Map Direction

-------------------------------------------
-- Model
-------------------------------------------

makeWorld "Model" [''Camera, ''Position, ''Velocity, ''Direction]

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
        EventKey (SpecialKey KeySpace) Down _ _ -> cmap (\(Pos _) -> (Pos (0, 0), Vel (0, 0), DirectionStationary))
        EventKey (Char 'h') Down _ _ -> cmap (const DirectionLeft :: Direction -> Direction)
        EventKey (Char 'j') Down _ _ -> cmap (const DirectionRight :: Direction -> Direction)
        EventKey (Char 'k') Down _ _ -> cmap (const DirectionUp :: Direction -> Direction)
        EventKey (Char 'l') Down _ _ -> cmap (const DirectionDown :: Direction -> Direction)
        EventKey (Char _) Up _ _ -> cmap (const DirectionStationary :: Direction -> Direction)
        _ -> pure ()

stepPos :: Float -> System' ()
stepPos dt =
        cmap
                ( \(Pos (x, y), Vel (vX, vY)) ->
                        -- search for all entities with pos and vel attributes, update
                        (Pos (x + vX * dt, y + vY * dt))
                )

stepVel :: Float -> System' ()
stepVel dt =
        cmap
                ( \(d, Vel (vX, vY)) -> case d of
                        DirectionLeft -> Vel (10, vY)
                        DirectionRight -> Vel (-10, vY)
                        DirectionUp -> Vel (vX, 10)
                        DirectionDown -> Vel (vX, -10)
                        DirectionStationary -> Vel (0, 0)
                )

-------------------------------------------
-- Spawn entities
-------------------------------------------

createEntities :: System' ()
createEntities = do
        newEntity_ (Pos (0, 0), Vel (0, 0), DirectionStationary)

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
                        stepVel dt
                )
    where
        display = InWindow "Basic" (1024, 1024) (10, 10)

oppentimerMain :: IO ()
oppentimerMain = do
        w <- initModel
        runWith w game
