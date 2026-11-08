{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Oppentimer (oppentimerMain) where

import Apecs hiding (Set)
import Apecs.Gloss
import qualified Data.Map as M
import Data.Set (Set, member, singleton)
import Graphics.Gloss (BitmapData (bitmapSize))
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Oppentimer.Audio (haskellPlayAudio)
import Oppentimer.Text (centerText)
import Oppentimer.Timer (Timer, isExpired, newTimer, tickTimer)
import System.Exit (exitSuccess)
import System.Random (Random (randomR), StdGen, mkStdGen)

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

data TimeToLive = TimeToLive Float
instance Component TimeToLive where type Storage TimeToLive = Map TimeToLive

data Harry = Harry
instance Component Harry where type Storage Harry = Map Harry

data RoadmanSpawner = RoadmanSpawner Float Texture StdGen
instance Component RoadmanSpawner where type Storage RoadmanSpawner = Map RoadmanSpawner

data Roadman = Roadman
instance Component Roadman where type Storage Roadman = Map Roadman

data Scene = BattleOppenheimer | Death | Win | TitleScreen deriving (Eq, Ord)
instance Component Scene where type Storage Scene = Map Scene

data VisibleInScenes = VisibleInScenes (Set Scene)
instance Component VisibleInScenes where type Storage VisibleInScenes = Map VisibleInScenes

data Texture = Texture Picture deriving (Show)
instance Component Texture where type Storage Texture = Map Texture

data CenteredText = CenteredText String Color Float deriving (Show)
instance Component CenteredText where type Storage CenteredText = Map CenteredText

arenaWidth, arenaHeight, arenaX, arenaY, playerWidth, roadmanWidth :: Float
arenaWidth = 300
arenaHeight = 150
arenaX = 0
arenaY = -80
roadmanWidth = 20
playerWidth = 20

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
        , ''RoadmanSpawner
        , ''Roadman
        , ''Harry
        , ''Collidable
        , ''TimeToLive
        , ''Scene
        , ''VisibleInScenes
        , ''CenteredText
        , ''Timer
        ]

type System' a = System Model a

type DestructableComponents =
        ( Direction
        , Texture
        , Wall
        , RoadmanSpawner
        , Roadman
        , Harry
        , Collidable
        , ( TimeToLive
          , Scene
          , VisibleInScenes
          , Timer
          , Pos
          , Vel
          )
        )

-------------------------------------------
-- View
-------------------------------------------

view :: System' Picture
view = do
        foldDrawM
                ( \(scene :: Scene) -> do
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
                )

-------------------------------------------
-- Controller
-------------------------------------------

directionBinds :: M.Map Key Direction
directionBinds =
        M.fromList
                [ (Char 'h', DirectionLeft)
                , (Char 'a', DirectionLeft)
                , (SpecialKey KeyLeft, DirectionLeft)
                , (Char 'k', DirectionUp)
                , (Char 'w', DirectionUp)
                , (SpecialKey KeyUp, DirectionUp)
                , (Char 'j', DirectionDown)
                , (Char 's', DirectionDown)
                , (SpecialKey KeyDown, DirectionDown)
                , (Char 'l', DirectionRight)
                , (Char 'd', DirectionRight)
                , (SpecialKey KeyRight, DirectionRight)
                ]

inputControllerBattle :: Event -> System' ()
inputControllerBattle event = case event of
        EventKey (SpecialKey KeyEsc) Down _ _ ->
                liftIO exitSuccess
        EventKey (Char 'q') Down _ _ -> liftIO exitSuccess
        EventKey (Char '`') Down _ _ -> cmap (const Win :: Scene -> Scene)
        EventKey (k) Down _ _ -> case k `M.lookup` directionBinds of
                Just bind -> cmap (const bind :: Direction -> Direction)
                Nothing -> pure ()
        EventKey _ Up _ _ -> cmap (const DirectionStationary :: Direction -> Direction)
        _ -> pure ()

inputControllerTitleScreen :: Event -> System' ()
inputControllerTitleScreen event = case event of
        EventKey (SpecialKey KeyEsc) Down _ _ ->
                liftIO exitSuccess
        EventKey (Char 'q') Down _ _ -> liftIO exitSuccess
        EventKey (Char 'c') Down _ _ -> cmap (const BattleOppenheimer :: Scene -> Scene)
        _ -> pure ()

inputControllerDeath :: Event -> System' ()
inputControllerDeath event = case event of
        EventKey (SpecialKey KeyEsc) Down _ _ ->
                liftIO exitSuccess
        EventKey (Char 'q') Down _ _ -> liftIO exitSuccess
        EventKey (Char 'c') Down _ _ -> do
                resetEntities
                cmap (const BattleOppenheimer :: Scene -> Scene)
        _ -> pure ()

stepHarryPhysics :: Float -> System' ()
stepHarryPhysics dt = do
        cmap
                ( \(d, Vel _ _, Harry) -> case d of
                        DirectionLeft -> Vel (-velocity) 0
                        DirectionRight -> Vel velocity 0
                        DirectionUp -> Vel 0 velocity
                        DirectionDown -> Vel 0 (-velocity)
                        DirectionStationary -> Vel 0 0
                )
        cmapM
                ( \(Pos x y, Collidable (Pos _ _) harrySize, Vel vX vY, Harry, harryId :: Entity) -> do
                        let newPos = Pos (x + vX * dt) (y + vY * dt)
                        let newHarryBox = Collidable newPos harrySize
                        isCollision <- cfold (\acc (box, id' :: Entity) -> acc || (id' /= harryId && newHarryBox `collidesWith` box)) False
                        if isCollision
                                then pure (Pos x y, Vel 0 0)
                                else pure (newPos, Vel vX vY)
                )
        cmapM
                ( \(harryBox :: Collidable, Harry, harryId :: Entity) -> do
                        isCollision <- cfold (\acc (box, Roadman, id' :: Entity) -> acc || (id' /= harryId && harryBox `collidesWith` box)) False
                        if isCollision
                                then cmap (const Death :: Scene -> Scene)
                                else pure ()
                )
    where
        velocity = 50

addVec :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVec (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

hypotVec :: (Floating a) => (a, a) -> a
hypotVec (x, y) = sqrt ((x ** 2) + (y ** 2))

multVec :: (Num a) => a -> (a, a) -> (a, a)
multVec s (x, y) = (x * s, y * s)

negateVec :: (Num a) => (a, a) -> (a, a)
negateVec = multVec (-1)

normaliseVec :: (Num a, Floating a) => (a, a) -> (a, a)
normaliseVec v = (1 / hypotVec v) `multVec` v

stepRoadmanSpawners :: Float -> System' ()
stepRoadmanSpawners dt =
        cmapM
                ( \(Harry, Pos harryX harryY) ->
                        cmapM
                                ( \(RoadmanSpawner t roadmanBmp gen) -> do
                                        if t > 0
                                                then pure $ RoadmanSpawner (t - dt) roadmanBmp gen
                                                else
                                                        let
                                                                harryPos = (harryX, harryY)
                                                                (r :: Float, gen') = randomR (80, 200) gen
                                                                (theta :: Float, gen'') = randomR (0, 2 * pi) gen'
                                                                offset = (r * sin theta, r * cos theta)
                                                                roadmanPos = uncurry Pos $ offset `addVec` harryPos
                                                                normalisedVec = normaliseVec (negateVec offset)
                                                         in
                                                                newEntity_
                                                                        ( roadmanPos
                                                                        , uncurry Vel $ 80 `multVec` normalisedVec
                                                                        , roadmanBmp
                                                                        , VisibleInScenes (singleton BattleOppenheimer)
                                                                        , Collidable roadmanPos (Size roadmanWidth roadmanWidth)
                                                                        , Roadman
                                                                        , TimeToLive 10
                                                                        )
                                                                        >> (pure $ RoadmanSpawner (0.5) roadmanBmp gen'')
                                )
                )

stepTimeToLive :: Float -> System' ()
stepTimeToLive dt = do
        cmap (\(TimeToLive t) -> TimeToLive (t - dt))
        cmapM_
                ( \(TimeToLive t, e :: Entity) ->
                        if t <= 0
                                then cmapM (\(_ :: TimeToLive, e' :: Entity) -> if e' == e then destroy e (Proxy @DestructableComponents) else pure ())
                                else pure ()
                )

stepClockText :: System' ()
stepClockText = cmap (\(timer :: Timer) -> CenteredText (show timer) white 0.1)

stepRoadmanPhysics :: Float -> System' ()
stepRoadmanPhysics dt =
        cmap (\((Vel vX vY, Pos x y)) -> Pos (x + vX * dt) (y + vY * dt))

stepCollisionBox :: System' ()
stepCollisionBox =
        cmap
                ( \(p :: Pos, Collidable _ size) ->
                        Collidable p size
                )

stepCheckWin :: System' ()
stepCheckWin =
        cmapM
                ( \(timer :: Timer) ->
                        if isExpired timer
                                then cmapM (\(_ :: Scene) -> pure Win)
                                else pure ()
                )

stepCenteredText :: System' ()
stepCenteredText =
        cmapM
                ( \(CenteredText s color' scale') -> do
                        liftIO $ fmap (Texture . color color' . scale scale' scale') . centerText $ s
                )

stepTimers :: Float -> System' ()
stepTimers dt = cmap (tickTimer dt)

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

loadSprite :: Float -> String -> IO SpriteData
loadSprite scale' path = do
        imageBmp <-
                ( \bmp -> case bmp of
                        Just (Bitmap bmp') -> bmp'
                        _ -> (error "failed to load")
                )
                        <$> (loadJuicyPNG path)
        let imageWidth = scale'
        let imageScale = imageWidth / (fromIntegral (fst $ bitmapSize imageBmp) :: Float)
        let imageHeight = imageScale * (fromIntegral . snd $ bitmapSize imageBmp)
        let imageBmp' = scale imageScale imageScale $ bitmap imageBmp

        pure $ SpriteData (Size imageWidth imageHeight) (Texture imageBmp')

resetEntities :: System' ()
resetEntities = do
        cmapM (\(Roadman, e :: Entity) -> destroy e (Proxy @DestructableComponents))
        cmap
                ( \(Harry) ->
                        ( Pos arenaX arenaY
                        , Vel 0 0
                        , DirectionStationary
                        )
                )

createEntities :: System' ()
createEntities = do
        -- set the scene
        newEntity_ TitleScreen

        ---------------------------------------
        ---          battle scene           ---
        ---------------------------------------
        -- oppenheimer
        SpriteData (Size _oppenheimerWidth _oppenheimerHeight) (Texture oppenheimerBmp) <- liftIO $ loadSprite 250 "assets/characters/oppenheimer.png"
        newEntity_
                ( Pos 0 70
                , VisibleInScenes (singleton BattleOppenheimer)
                , Texture oppenheimerBmp
                )

        -- arena sprite
        newEntity_
                ( Pos arenaX arenaY
                , Texture (color white $ rectangleWire arenaWidth arenaHeight)
                , VisibleInScenes (singleton BattleOppenheimer)
                )

        -- arena walls
        newEntity_ -- north wall
                ( Pos (arenaX) (arenaY + (arenaHeight / 2))
                , Wall
                , Collidable (Pos (arenaX) (arenaY + (arenaHeight / 2))) (Size arenaWidth 0)
                , VisibleInScenes (singleton BattleOppenheimer)
                )

        newEntity_ -- east wall
                ( Pos (arenaX - (arenaWidth / 2)) (arenaY)
                , Wall
                , Collidable (Pos (arenaX - (arenaWidth / 2)) (arenaY)) (Size 0 arenaHeight)
                , VisibleInScenes (singleton BattleOppenheimer)
                )

        newEntity_ -- west wall
                ( Pos (arenaX + (arenaWidth / 2)) (arenaY)
                , Wall
                , Collidable (Pos (arenaX + (arenaWidth / 2)) (arenaY)) (Size 0 arenaHeight)
                , VisibleInScenes (singleton BattleOppenheimer)
                )

        newEntity_ -- south wall
                ( Pos (arenaX) (arenaY - (arenaHeight / 2))
                , Wall
                , Collidable (Pos (arenaX) (arenaY - (arenaHeight / 2))) (Size arenaWidth 0)
                , VisibleInScenes (singleton BattleOppenheimer)
                )

        -- harry
        SpriteData (Size harryWidth harryHeight) (Texture harryBmp) <- liftIO $ loadSprite playerWidth "assets/characters/harry.png"
        newEntity_
                ( Harry
                , Texture harryBmp
                , Collidable (Pos arenaX arenaY) (Size harryWidth harryHeight)
                , VisibleInScenes (singleton BattleOppenheimer)
                )

        -- roadman
        SpriteData (Size _roadmanWidth _roadmanHeight) (Texture roadmanBmp) <- liftIO $ loadSprite roadmanWidth "assets/characters/roadman.png"
        newEntity_
                (RoadmanSpawner 0 (Texture roadmanBmp) (mkStdGen 0))

        -- oppentimer
        newEntity_
                ( Pos 0 150
                , newTimer (5 * 60)
                , VisibleInScenes (singleton BattleOppenheimer)
                )

        ---------------------------------------
        ---          death scene            ---
        ---------------------------------------
        -- death scene text

        newEntity_
                ( Pos 0 100
                , CenteredText "You are dead" white 0.1
                , VisibleInScenes (singleton Death)
                )

        newEntity_
                ( Pos 0 0
                , CenteredText "Oppentimer" white 0.2
                , VisibleInScenes (singleton TitleScreen)
                )

        -- battle pass
        SpriteData (Size _battlePassWidth _battlePassHeight) (Texture battlePassBmp) <- liftIO $ loadSprite 300 "assets/other/BATTLEPASS.png"
        newEntity_
                ( Pos 0 0
                , Texture battlePassBmp
                , VisibleInScenes (singleton Death)
                )

        resetEntities

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
                        -- p2 <- viewHitboxes
                        pure $ pictures [p1]
                )
                ( \event -> do
                        cmapM_
                                ( \(scene :: Scene) -> do
                                        case scene of
                                                BattleOppenheimer -> inputControllerBattle event
                                                TitleScreen -> inputControllerTitleScreen event
                                                Death -> inputControllerDeath event
                                                Win -> pure ()
                                )
                )
                ( \dt -> do
                        cmapM_
                                ( \(scene :: Scene) -> do
                                        case scene of
                                                BattleOppenheimer -> do
                                                        stepHarryPhysics dt
                                                        stepHarryPhysics dt
                                                        stepRoadmanPhysics dt
                                                        stepRoadmanSpawners dt
                                                        stepTimeToLive dt
                                                        stepTimers dt
                                                        stepClockText
                                                        stepCheckWin
                                                        stepCollisionBox
                                                _ -> pure ()
                                )
                        stepCenteredText
                )
    where
        display = InWindow "Basic" (1024, 1024) (10, 10)

oppentimerMain :: IO ()
oppentimerMain = do
        w <- initModel
        _ <- haskellPlayAudio "src/AUDIO_FFI_STUFF/foxy-scream-fnaf-1.mp3"
        runWith w game
