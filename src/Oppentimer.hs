{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Oppentimer (oppentimerMain) where

import Apecs hiding (Set)
import Apecs.Gloss
import Data.Set (Set, fromList, member, singleton)
import Graphics.Gloss (BitmapData (bitmapSize))
import Graphics.Gloss.Juicy (loadJuicyPNG)
import System.Exit (exitSuccess)

-- import Oppentimer.Clock (Clock)

-------------------------------------------
-- Components
-------------------------------------------

data SpriteData = SpriteData Size Texture

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

data Scene = BattleDan | Death deriving (Eq, Ord)
instance Component Scene where type Storage Scene = Map Scene

data VisibleInScenes = VisibleInScenes (Set Scene)
instance Component VisibleInScenes where type Storage VisibleInScenes = Map VisibleInScenes

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
        , ''Scene
        , ''VisibleInScenes
        -- , ''Clock
        ]

type System' a = System Model a

-------------------------------------------
-- View
-------------------------------------------

view :: System' Picture
view =
        foldDrawM
                ( \(scene :: Scene) ->
                        foldDraw -- cram everything into one picture
                                ( \(Pos x y, Texture bmp, VisibleInScenes entityScene) ->
                                        if scene `member` entityScene
                                                then translate x y $ bmp
                                                else Blank
                                )
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
        EventKey (Char '`') Down _ _ -> cmap (const Death :: Scene -> Scene)
        EventKey (Char _) Up _ _ -> cmap (const DirectionStationary :: Direction -> Direction)
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
                ( \(d, Vel _ _) -> case d of
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

-- createBattleSceneEntities :: System' ()
-- createBattleSceneEntities  = do

loadSprite :: String -> IO SpriteData
loadSprite path = do
        harryBmp <-
                ( \bmp -> case bmp of
                        Just (Bitmap bmp') -> bmp'
                        _ -> (error "failed to load")
                )
                        <$> (loadJuicyPNG path)
        let harryWidth = 20 :: Float
        let harryScale = harryWidth / fromIntegral (fst $ bitmapSize harryBmp)
        let harryHeight = harryScale * (fromIntegral . snd $ bitmapSize harryBmp)
        let harryBmp' = scale harryScale harryScale $ bitmap harryBmp

        pure $ SpriteData (Size harryWidth harryHeight) (Texture harryBmp')

createEntities :: System' ()
createEntities = do
        -- set the scene
        newEntity_ BattleDan

        ---------------------------------------
        ---          battle scene           ---
        ---------------------------------------
        -- arena sprite
        newEntity_
                ( Pos arenaX arenaY
                , Texture (color white $ rectangleWire arenaWidth arenaHeight)
                , VisibleInScenes (singleton BattleDan)
                )

        -- arena walls
        newEntity_ -- north wall
                ( Pos (arenaX) (arenaY + (arenaHeight / 2))
                , Wall
                , Collidable (Pos (arenaX) (arenaY + (arenaHeight / 2))) (Size arenaWidth 0)
                , VisibleInScenes (singleton BattleDan)
                )

        newEntity_ -- east wall
                ( Pos (arenaX - (arenaWidth / 2)) (arenaY)
                , Wall
                , Collidable (Pos (arenaX - (arenaWidth / 2)) (arenaY)) (Size 0 arenaHeight)
                , VisibleInScenes (singleton BattleDan)
                )

        newEntity_ -- west wall
                ( Pos (arenaX + (arenaWidth / 2)) (arenaY)
                , Wall
                , Collidable (Pos (arenaX + (arenaWidth / 2)) (arenaY)) (Size 0 arenaHeight)
                , VisibleInScenes (singleton BattleDan)
                )

        newEntity_ -- south wall
                ( Pos (arenaX) (arenaY - (arenaHeight / 2))
                , Wall
                , Collidable (Pos (arenaX) (arenaY - (arenaHeight / 2))) (Size arenaWidth 0)
                , VisibleInScenes (singleton BattleDan)
                )

        -- harry
        SpriteData (Size harryWidth harryHeight) (Texture harryBmp) <- liftIO $ loadSprite "assets/characters/harry.png"
        newEntity_
                ( Pos (arenaX) (arenaY)
                , Harry
                , Vel 0 0
                , Texture harryBmp
                , DirectionStationary
                , Collidable (Pos arenaX arenaY) (Size harryWidth harryHeight)
                , VisibleInScenes (singleton BattleDan)
                )

        -- oppentimer
        -- newEntity_
        --        ( Pos 0 0
        --        , Texture (translate 100 100 $ scale 0.1 0.1 $ color white $ text "4:59")
        -- 	,
        --        )
        ---------------------------------------
        ---          death scene            ---
        ---------------------------------------
        -- death scene text
        newEntity_
                ( Pos 0 0
                , Texture (translate 100 100 $ scale 0.1 0.1 $ color white $ text "You are dead")
                , VisibleInScenes (singleton Death)
                )
    where
        arenaWidth = 200
        arenaHeight = 100
        arenaX = 0
        arenaY = -100

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
