{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Oppentimer (oppentimerMain) where

import Apecs
import Apecs.Gloss
import Data.Maybe (fromMaybe)
import Graphics.Gloss.Juicy (loadJuicyPNG)

-- import Oppentimer.Clock (Clock)

import Control.Monad (join)
import System.Exit (exitSuccess)

-------------------------------------------
-- Components
-------------------------------------------

data Pos = Pos Float Float
instance Component Pos where type Storage Pos = Map Pos

data Vel = Vel Float Float
instance Component Vel where type Storage Vel = Map Vel

data Direction = DirectionLeft | DirectionRight | DirectionUp | DirectionDown | DirectionStationary
instance Component Direction where type Storage Direction = Map Direction

data Wall = Wall
instance Component Wall where type Storage Wall = Map Wall

data Size = Size Float Float

data Collidable = Collidable Pos Size
instance Component Collidable where type Storage Collidable = Map Collidable

data Harry = Harry
instance Component Harry where type Storage Harry = Map Harry

newtype Texture = Texture Picture
instance Component Texture where type Storage Texture = Map Texture

-------------------------------------------
-- Model
-------------------------------------------

makeWorld
        "Model"
        [ ''Camera
        , ''Pos
        , ''Vel
        , ''Direction
        , ''Texture
        , ''Wall
        , ''Harry
        , ''Collidable
        -- , ''Clock
        ]

type System' a = System Model a

-------------------------------------------
-- View
-------------------------------------------

view :: System' Picture
view =
        foldDraw -- cram everything into one picture
                ( \(Pos x y, Texture bmp) ->
                        translate x y $ bmp
                )

viewHitboxes :: System' Picture
viewHitboxes =
        foldDraw -- hitboxes and stuff
                ( \(Collidable (Pos x y) (Size w h)) ->
                        translate x y $ color red $ lineLoop (rectanglePath w h)
                        -- , Texture (color white $ lineLoop $ rectanglePath arenaWidth arenaHeight)
                )

-------------------------------------------
-- Controller
-------------------------------------------

inputController :: Event -> System' ()
inputController event = case event of
        EventKey (SpecialKey KeyEsc) Down _ _ ->
                liftIO exitSuccess
        EventKey (Char 'q') Down _ _ -> liftIO exitSuccess
        EventKey (Char 'h') Down _ _ -> cmap (const DirectionLeft :: Direction -> Direction)
        EventKey (Char 'j') Down _ _ -> cmap (const DirectionDown :: Direction -> Direction)
        EventKey (Char 'k') Down _ _ -> cmap (const DirectionUp :: Direction -> Direction)
        EventKey (Char 'l') Down _ _ -> cmap (const DirectionRight :: Direction -> Direction)
        EventKey (Char '`') Down _ _ -> cmap (const DirectionStationary :: Direction -> Direction)
        _ -> pure ()

stepPos :: Float -> System' ()
stepPos dt =
        cmapM
                ( \(Collidable (Pos x y) harrySize, Vel vX vY, Harry, harryId :: Entity) -> do
                        let newPos = Pos (x + vX * dt) (y + vY * dt)
                        let newHarryBox = Collidable newPos harrySize
                        isCollision <- cfold (\acc (box :: Collidable, id' :: Entity) -> acc || (id' /= harryId && collidesWith newHarryBox box)) False
                        if isCollision
                                then pure (Pos x y, Vel 0 0)
                                else pure (newPos, Vel vX vY)
                )

stepCollisionBox :: System' ()
stepCollisionBox =
        cmap
                ( \(p :: Pos, Collidable _ size) ->
                        Collidable p size
                )

stepVel :: System' ()
stepVel =
        cmap
                ( \(d, Vel vX vY) -> case d of
                        DirectionLeft -> Vel (-velocity) 0
                        DirectionRight -> Vel velocity 0
                        DirectionUp -> Vel 0 velocity
                        DirectionDown -> Vel 0 (-velocity)
                        DirectionStationary -> Vel 0 0
                )
    where
        velocity = 50

-- checkArenaCollision :: Float -> System' ()
-- checkArenaCollision dt =
--        cmapM_
--                ( \(Harry, harryId :: Entity) -> do
-- 			collides <- collect (\(c :: Collidable, id :: Entity) -> if id /= harryId && collidesWith  then)
--                        pure ()
--                )

collidesWith :: Collidable -> Collidable -> Bool
collidesWith
        (Collidable (Pos acx acy) (Size aw ah))
        (Collidable (Pos bcx bcy) (Size bw bh)) =
                (ax < bx + bw) && (ax + aw > bx) && (ay < by + bh) && (ay + ah > by)
            where
                ax = acx - aw / 2
                ay = acy - ah / 2
                bx = bcx - bw / 2
                by = bcy - bh / 2

-- >>> collidesWith (Collidable (Pos 0 0) (Size 10 10)) (Collidable (Pos 5 5) (Size 1 1))
-- True

-- >>> collidesWith (Collidable (Pos 0 0) (Size 10 10)) (Collidable (Pos 7 7) (Size 1 1))
-- False

-- >>> collidesWith (Collidable (Pos (-1) (-1)) (Size 10 10)) (Collidable (Pos 5 5) (Size 11 11))
-- True

-- >>> collidesWith (Collidable (Pos (-1) (-1)) (Size 10 10)) (Collidable (Pos 10 10) (Size 11 11))
-- False

-------------------------------------------
-- Spawn entities
-------------------------------------------

createEntities :: System' ()
createEntities = do
        harryBmp <- (fromMaybe (error "failed to load")) <$> (liftIO $ loadJuicyPNG "assets/characters/harry.png")

        -- arena sprite
        newEntity_
                ( Pos arenaX arenaY
                , Texture (color white $ rectangleWire arenaWidth arenaHeight)
                )

        -- arena walls
        newEntity_ -- north wall
                ( Pos (arenaX) (arenaY + (arenaHeight / 2))
                , Wall
                , Collidable (Pos (arenaX) (arenaY + (arenaHeight / 2))) (Size arenaWidth 10)
                )
        newEntity_ -- east wall
                ( Pos (arenaX - (arenaWidth / 2)) (arenaY)
                , Wall
                , Collidable (Pos (arenaX - (arenaWidth / 2)) (arenaY)) (Size 10 arenaHeight)
                )
        newEntity_ -- west wall
                ( Pos (arenaX + (arenaWidth / 2)) (arenaY)
                , Wall
                , Collidable (Pos (arenaX + (arenaWidth / 2)) (arenaY)) (Size 10 arenaHeight)
                )
        newEntity_ -- south wall
                ( Pos (arenaX) (arenaY - (arenaHeight / 2))
                , Wall
                , Collidable (Pos (arenaX) (arenaY - (arenaHeight / 2))) (Size arenaWidth 10)
                )

        -- harry
        newEntity_
                ( Pos 0 0
                , Harry
                , Vel 0 0
                , Texture (scale 0.1 0.1 harryBmp)
                , DirectionStationary
                , Collidable (Pos (-10) (-10)) (Size 20 20)
                )

        -- oppentimer
        newEntity_
                ( Pos 0 0
                , Texture (color white $ text "4:59")
                )
    where
        arenaWidth = 400
        arenaHeight = 200
        arenaX = 0
        arenaY = 0

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
                60 -- fps
                ( do
                        p1 <- view
                        p2 <- viewHitboxes
                        pure $ pictures [p1, p2]
                )
                inputController
                ( \dt -> do
                        stepPos dt
                        stepVel
                        stepCollisionBox
                )
    where
        display = InWindow "Basic" (1024, 1024) (10, 10)

oppentimerMain :: IO ()
oppentimerMain = do
        w <- initModel
        runWith w game
