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
import qualified Data.Set as S
import Graphics.Gloss (BitmapData (bitmapSize))
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Oppentimer.Audio (haskellPlayAudio)
import Oppentimer.Text (centerText)
import Oppentimer.Timer (Timer, getTime, isExpired, newTimer, tickTimer)
import Oppentimer.Vector (addVec, multVec, negateVec, normaliseVec)
import System.Exit (exitSuccess)
import System.Random (Random (randomR), StdGen, initStdGen)

-------------------------------------------
-- Components
-------------------------------------------

data SpriteData = SpriteData Size Texture

data Pos = Pos Float Float
instance Component Pos where type Storage Pos = Map Pos

data Vel = Vel Float Float
instance Component Vel where type Storage Vel = Map Vel

data Direction = DirectionLeft | DirectionRight | DirectionUp | DirectionDown | DirectionStationary deriving (Show, Eq)
instance Component Direction where type Storage Direction = Map Direction

data Wall = Wall
instance Component Wall where type Storage Wall = Map Wall

data Size = Size Float Float

data Collidable = Collidable Size
instance Component Collidable where type Storage Collidable = Map Collidable

data TimeToLive = TimeToLive Float
instance Component TimeToLive where type Storage TimeToLive = Map TimeToLive

data HarryState = HarryNormal | HarryEvil Int

harryStateHit :: HarryState -> HarryState
harryStateHit HarryNormal = HarryNormal
harryStateHit (HarryEvil 0) = HarryNormal
harryStateHit (HarryEvil remainingHits) = HarryEvil (remainingHits - 1)

data Harry = Harry
        { harryNormalTexture :: Texture
        , harryEvilTexture :: Texture
        , harryState :: HarryState
        }
instance Component Harry where type Storage Harry = Map Harry

data Wallace = Wallace
instance Component Wallace where type Storage Wallace = Map Wallace

data RoadmanSpawner = RoadmanSpawner
        { spawnerDelay :: Float
        , spawnerRoadmanTexture :: Texture
        , spawnerWallaceTexture :: Texture
        , spawnerGen :: StdGen
        }

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

arenaWidth, arenaHeight, arenaX, arenaY, playerWidth, roadmanWidth, survivalTime, harryVelocity :: Float
arenaWidth = 300
arenaHeight = 150
arenaX = 0
arenaY = -80
roadmanWidth = 10
playerWidth = 20
survivalTime = 5 * 60
harryVelocity = 50

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
        , ''Wallace
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
          , CenteredText
          , Wallace
          )
        )

-------------------------------------------
-- View
-------------------------------------------

view :: System' Picture
view = do
        foldDrawM
                ( \(scene :: Scene) -> do
                        p1 <-
                                foldDraw
                                        ( \(Pos x y, timer :: Timer, VisibleInScenes entityScenes) ->
                                                if scene `member` entityScenes
                                                        then
                                                                translate x y $
                                                                        color red $
                                                                                rectangleSolid
                                                                                        (getTime timer / survivalTime * 300 + 100)
                                                                                        75
                                                        else Blank
                                        )
                        p2 <-
                                foldDraw -- cram everything into one picture
                                        ( \(Pos x y, Texture bmp, VisibleInScenes entityScenes) ->
                                                if scene `member` entityScenes
                                                        then translate x y $ bmp
                                                        else Blank
                                        )
                        pure $ pictures [p1, p2]
                )

viewHitboxes :: System' Picture
viewHitboxes =
        foldDraw -- hitboxes and stuff
                ( \(Pos x y, Collidable (Size w h)) ->
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
        EventKey k Down _ _ -> case k `M.lookup` directionBinds of
                Just bind -> cmap (const bind :: Direction -> Direction)
                Nothing -> pure ()
        EventKey k Up _ _ ->
                cmap
                        ( \currentDir ->
                                if k `M.lookup` directionBinds == Just currentDir
                                        then
                                                DirectionStationary
                                        else
                                                currentDir
                        )
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

inputControllerWin :: Event -> System' ()
inputControllerWin = inputControllerDeath

stepHarryPhysics :: Float -> System' ()
stepHarryPhysics dt = do
        cmap
                ( \(d, Vel _ _, _ :: Harry) -> case d of
                        DirectionLeft -> Vel (-harryVelocity) 0
                        DirectionRight -> Vel harryVelocity 0
                        DirectionUp -> Vel 0 harryVelocity
                        DirectionDown -> Vel 0 (-harryVelocity)
                        DirectionStationary -> Vel 0 0
                )
        cmapM
                ( \(harryPos :: Pos, harryBox, Vel vX vY, harry :: Harry, harryId :: Entity) -> do
                        -- collide with walls
                        _ <- do
                                let Pos x y = harryPos
                                let newPos = Pos (x + vX * dt) (y + vY * dt)
                                isCollision <-
                                        cfold
                                                ( \acc (pos, box, id' :: Entity) ->
                                                        acc || (id' /= harryId && (newPos, harryBox) `collidesWith` (pos, box))
                                                )
                                                False
                                if isCollision
                                        then pure (Pos x y, Vel 0 0)
                                        else pure (newPos, Vel vX vY)

                        -- collide with roadmen
                        cmapM_
                                ( \(pos, box, Roadman, roadmanId :: Entity) -> do
                                        let collision = (harryPos, harryBox) `collidesWith` (pos, box)
                                        case harryState harry of
                                                HarryNormal ->
                                                        if collision
                                                                then cmap (const Death :: Scene -> Scene)
                                                                else pure ()
                                                HarryEvil _ ->
                                                        if collision
                                                                then do
                                                                        destroy roadmanId (Proxy @DestructableComponents)
                                                                        set harryId harry{harryState = harryStateHit $ harryState harry}
                                                                else pure ()
                                )
                        -- collide with wallace
                        cmapM_
                                ( \(pos, box, Wallace, wallaceId :: Entity) -> do
                                        let collision = (harryPos, harryBox) `collidesWith` (pos, box)
                                        if collision
                                                then do
                                                        destroy wallaceId (Proxy @DestructableComponents)
                                                        set harryId harry{harryState = HarryEvil 3}
                                                else pure ()
                                )
                )
        cmap
                ( \(harry :: Harry) -> do
                        case harryState harry of
                                HarryNormal -> (harryNormalTexture harry)
                                HarryEvil _ -> (harryEvilTexture harry)
                )

stepRoadmanSpawners :: Float -> System' ()
stepRoadmanSpawners dt =
        cmapM
                ( \(_ :: Harry, Pos harryX harryY) ->
                        cmapM
                                ( \(spawner) -> do
                                        if spawnerDelay spawner > 0
                                                then pure $ spawner{spawnerDelay = (spawnerDelay spawner - dt)}
                                                else
                                                        let
                                                                harryPos = (harryX, harryY)
                                                                gen = spawnerGen spawner
                                                                (r :: Float, gen') = randomR (200, 300) gen
                                                                (theta :: Float, gen'') = randomR (0, 2 * pi) gen'
                                                                (variant :: Float, gen''') = randomR (0, 1) gen''
                                                                offset = (r * sin theta, r * cos theta)
                                                                roadmanPos = uncurry Pos $ offset `addVec` harryPos
                                                                normalisedVec = normaliseVec (negateVec offset)
                                                         in
                                                                ( if variant < 0.1
                                                                        then
                                                                                newEntity_
                                                                                        ( roadmanPos
                                                                                        , uncurry Vel $ 80 `multVec` normalisedVec
                                                                                        , spawnerWallaceTexture spawner
                                                                                        , VisibleInScenes (singleton BattleOppenheimer)
                                                                                        , Collidable (Size roadmanWidth roadmanWidth)
                                                                                        , Wallace
                                                                                        , TimeToLive 10
                                                                                        )
                                                                        else
                                                                                newEntity_
                                                                                        ( roadmanPos
                                                                                        , uncurry Vel $ 80 `multVec` normalisedVec
                                                                                        , spawnerRoadmanTexture spawner
                                                                                        , VisibleInScenes (singleton BattleOppenheimer)
                                                                                        , Collidable (Size roadmanWidth roadmanWidth)
                                                                                        , Roadman
                                                                                        , TimeToLive 10
                                                                                        )
                                                                )
                                                                        >> (pure $ spawner{spawnerDelay = 0.5, spawnerGen = gen'''})
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

collidesWith :: (Pos, Collidable) -> (Pos, Collidable) -> Bool
collidesWith
        (Pos acx acy, Collidable (Size aw ah))
        (Pos bcx bcy, Collidable (Size bw bh)) =
                (ax < bx + bw) && (ax + aw > bx) && (ay < by + bh) && (ay + ah > by)
            where
                ax = acx - aw / 2
                ay = acy - ah / 2
                bx = bcx - bw / 2
                by = bcy - bh / 2

-- >>> collidesWith (Pos 0 0, Collidable (Size 10 10)) (Pos 5 5, Collidable (Size 1 1))
-- True

-- >>> collidesWith (Pos 0 0, Collidable (Size 10 10)) (Pos 7 7, Collidable (Size 1 1))
-- False

-- >>> collidesWith (Pos (-1) (-1), Collidable (Size 10 10)) (Pos 5 5, Collidable (Size 11 11))
-- True

-- >>> collidesWith (Pos (-1) (-1), Collidable (Size 10 10)) (Pos 10 10, Collidable (Size 11 11))
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
        cmapM (\(Wallace, e :: Entity) -> destroy e (Proxy @DestructableComponents))
        cmap
                ( \(harry :: Harry) ->
                        ( Pos arenaX arenaY
                        , Vel 0 0
                        , DirectionStationary
                        , harry{harryState = HarryNormal}
                        )
                )
        cmap (\(_ :: Timer) -> newTimer survivalTime)
        cmap (\(spawner :: RoadmanSpawner) -> spawner{spawnerDelay = 1.0})

createEntities :: System' ()
createEntities = do
        -- set the scene
        newEntity_ TitleScreen

        ---------------------------------------
        ---          battle scene           ---
        ---------------------------------------
        -- oppenheimer
        SpriteData
                (Size _oppenheimerWidth _oppenheimerHeight)
                (Texture oppenheimerBmp) <-
                liftIO $ loadSprite 250 "assets/characters/oppenheimer.png"
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
                , Collidable (Size arenaWidth 0)
                , VisibleInScenes (singleton BattleOppenheimer)
                )

        newEntity_ -- east wall
                ( Pos (arenaX - (arenaWidth / 2)) (arenaY)
                , Wall
                , Collidable (Size 0 arenaHeight)
                , VisibleInScenes (singleton BattleOppenheimer)
                )

        newEntity_ -- west wall
                ( Pos (arenaX + (arenaWidth / 2)) (arenaY)
                , Wall
                , Collidable (Size 0 arenaHeight)
                , VisibleInScenes (singleton BattleOppenheimer)
                )

        newEntity_ -- south wall
                ( Pos (arenaX) (arenaY - (arenaHeight / 2))
                , Wall
                , Collidable (Size arenaWidth 0)
                , VisibleInScenes (singleton BattleOppenheimer)
                )

        SpriteData
                harrySize
                (Texture harryNormalBmp) <-
                liftIO $ loadSprite playerWidth "assets/characters/harry.png"

        SpriteData
                _
                (Texture harryEvilBmp) <-
                liftIO $ loadSprite playerWidth "assets/characters/harry-evil.png"

        newEntity_
                ( Harry
                        { harryNormalTexture = Texture harryNormalBmp
                        , harryEvilTexture = Texture harryEvilBmp
                        , harryState = HarryNormal
                        }
                , Collidable harrySize
                , VisibleInScenes (singleton BattleOppenheimer)
                )

        SpriteData _ (Texture roadmanBmp) <- liftIO $ loadSprite roadmanWidth "assets/characters/roadman.png"
        SpriteData _ (Texture wallaceBmp) <- liftIO $ loadSprite roadmanWidth "assets/characters/ybj_wallace.png"
        gen <- liftIO initStdGen
        newEntity_
                RoadmanSpawner
                        { spawnerDelay = 0
                        , spawnerRoadmanTexture = (Texture roadmanBmp)
                        , spawnerWallaceTexture = (Texture wallaceBmp)
                        , spawnerGen = gen
                        }

        -- oppentimer
        newEntity_
                ( Pos 0 150
                , newTimer 0
                , VisibleInScenes (singleton BattleOppenheimer)
                )

        ---------------------------------------
        ---          death scene            ---
        ---------------------------------------
        -- death scene text

        newEntity_
                ( Pos 0 100
                , CenteredText "You got fairs-ed by a roadman :(" white 0.1
                , VisibleInScenes (singleton Death)
                )

        -- battle pass
        SpriteData
                (Size _battlePassWidth _battlePassHeight)
                (Texture battlePassBmp) <-
                liftIO $ loadSprite 300 "assets/other/BATTLEPASS.png"
        newEntity_
                ( Pos 0 0
                , Texture battlePassBmp
                , VisibleInScenes (singleton Death)
                )

        newEntity_
                ( Pos 0 75
                , CenteredText "OPPENTIMER" white 0.2
                , VisibleInScenes (singleton TitleScreen)
                )

        newEntity_
                ( Pos 0 75
                , CenteredText "YOU HAVE BECOME DEATH, DESTROYER OF WORLDS" white 0.1
                , VisibleInScenes (singleton Win)
                )

        newEntity_
                ( Pos 0 (-75)
                , CenteredText "PRESS c TO CONTINUE" white 0.1
                , VisibleInScenes (S.fromList [TitleScreen, Win])
                )

        SpriteData
                (Size _nuclearWidth _nuclearHeight)
                (Texture nuclearBmp) <-
                liftIO $ loadSprite 100 "assets/other/oppenheimer_bullet.png"
        newEntity_
                ( Pos 0 0
                , Texture nuclearBmp
                , VisibleInScenes (S.fromList [TitleScreen, Win])
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
                                                Win -> inputControllerWin event
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
