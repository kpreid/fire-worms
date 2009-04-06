-- This is a proof-of-concept/throwaway sketch of handling colliding circles
-- of different diameters (and therefore different masses).

import Data.IORef
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import System.IO
import IO (isEOFError)
import Control.Monad
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Unique
import System.Random
import Data.List

import Graphics.Rendering.OpenGL hiding (position)
import Graphics.UI.GLUT hiding (position)

n :: GLfloat -> GLfloat
n = id

myInit :: IO ()
myInit = do
   clearColor $= Color4 0.0 0.0 0.0 0.0
   shadeModel $= Flat

type MyVec = (GLfloat,GLfloat,GLfloat)

data Node = Node { key :: Unique,
                   position :: IORef MyVec,
                   velocity :: IORef MyVec,
                   radius :: GLfloat,
                   mass :: GLfloat,
                   label :: String } deriving Eq

instance Ord Node where a `compare` b = key a `compare` key b

data Graph = Graph { nodes :: S.Set Node }

data Report = Report { origin :: MyVec, 
                       marks :: [(MyVec,MyVec,GLfloat,GLfloat,GLfloat)] }

momentum node = do
  v <- readIORef (velocity node)
  return $ mul3 (mass node) v
  return (0, 0, magnitude3 $ mul3 (mass node) v)

ke node = do
  v <- readIORef (velocity node)
  return $ mass node * (magnitude3 v)**2 / 2
  --return (0, 0, magnitude3 $ mul3 (mass node) v)

totalMomentum graph = liftM (foldl' add3 (0,0,0)) $ mapM momentum (S.elems $ nodes graph)
totalKE graph = liftM sum $ mapM ke (S.elems $ nodes graph)

vertexMy (x,y,z) = vertex $ Vertex3 x y z

vertexRead p = vertexMy =<< readIORef p
vertexFromNode :: Node -> IO ()
vertexFromNode = vertexRead . position

circleFromNode :: Node -> IO ()
circleFromNode node = do
  (x,y,z) <- readIORef (position node)
  let r = radius node
  forM_ [0..49] $ \i -> do
    let t = i * (2*pi/50)
    vertex $ Vertex3 (x+r*cos t) (y+r*sin t) z

mapSetM_ f s = mapM_ f (S.elems s)
doSet = flip mapSetM_

font = Roman

type View = Double

--display :: IORef Graph -> IORef View -> DisplayCallback
display graphRef viewRef reportsRef = do
  clear [ColorBuffer]
  currentColor $= Color4 1.0 1.0 1.0 0.0
  preservingMatrix $ preservingAttrib [PointAttributes] $ do
    view <- readIORef viewRef

    rotate (realToFrac view) $ Vector3 (n 0) 1 0
    scale (n (recip boundary)) (recip boundary) (recip boundary)
    pointSize $= 3

    graph <- readIORef graphRef
    renderPrimitive Lines $ do
      doSet (nodes graph) circleFromNode
    renderPrimitive LineLoop $ do
      vertex $ Vertex3 (-boundary) (-boundary) 0
      vertex $ Vertex3 (-boundary) ( boundary) 0
      vertex $ Vertex3 ( boundary) ( boundary) 0
      vertex $ Vertex3 ( boundary) (-boundary) 0
    doSet (nodes graph) $ \node -> do
      (x,y,z) <- readIORef (position node)
      preservingMatrix $ do
        translate $ Vector3 x y z
        scale (n 0.03) 0.03 0.03
        w <- stringWidth font (label node)
        translate $ Vector3 (negate (fromIntegral w / 2 :: GLfloat)) 120 0
        renderString font (label node)
    reports <- readIORef reportsRef
    forM_ reports $ \(Report (x,y,z) ls) -> preservingMatrix $ do
      translate $ Vector3 x y z
      renderPrimitive Lines $ do
        forM_ ls $ \(v0,v1,r,g,b) -> do
        color $ Color3 r g b
        vertexMy v0
        vertexMy v1
    color $ Color3 (n 1) 1 1
      
    preservingMatrix $ do
      tm <- totalMomentum graph
      tk <- totalKE graph
      scale (n 0.03) 0.03 0.03
      translate $ Vector3 (-n 1000) 120 0
      preservingMatrix $ renderString font ("Total momentum: " ++ show tm)
      translate $ Vector3 (n 0) (-150) 0
      renderString font ("Total KE: " ++ show tk)
  swapBuffers


reshape :: ReshapeCallback
reshape s@(Size w h) = do
   viewport $= (Position 0 0, s)
   matrixMode $= Projection
   loadIdentity
   --perspective 60.0 (fromIntegral w / fromIntegral h) 1.0 20.0
   matrixMode $= Modelview 0
   loadIdentity
   --lookAt (Vertex3 0 0 5) (Vertex3 0 0 0) (Vector3 0 1 0)

newNode label pos vel radius = do
    posRef <- newIORef $ pos
    velRef <- newIORef $ vel
    key <- newUnique
    return $ Node { key = key,
                    position = posRef,
                    velocity = velRef,
                    label = label,
                    radius = radius,
                    mass = radius ** 2 }

subtract3 (x,y,z) (x',y',z') = (x'-x,y'-y,z'-z)
add3 (x,y,z) (x',y',z') = (x'+x,y'+y,z'+z)
mul3 f (x,y,z) = (x*f,y*f,z*f)
magnitude3 (x,y,z) = sqrt (x*x + y*y + z*z)

second3 f (x,y,z) = (x, f y, z)
first3 f (x,y,z) = (f x, y, z)

atan23 (x,y,_) = atan2 y x
rotate3 th (x,y,z) = (x*cos th - y*sin th,
                      x*sin th + y*cos th,
                      z)

inversePower exp factor = (* factor) . recip . (max 0.01) . (** exp)

affect node current point rule = do
  let rel = subtract3 current point
  modifyIORef (position node) $ \v ->
    add3 v (mul3 (rule $ (magnitude3 rel)) rel)

--doOneCommand nameMapRef fieldRef command = do
--  case command of
--    CNode name label -> do
--      [w1,w2,w3] <- sequence $ replicate 3 $ randomRIO (-1,1)
--      node <- newNode label (w1,w2,w3)
--      modifyIORef nameMapRef $ M.insert name node
--      modifyIORef fieldRef (\graph -> graph {
--        nodes = node `S.insert` (nodes graph) })
--    CEdge nameA nameB -> do
--      nameMap <- readIORef nameMapRef
--      a <- M.lookup nameA nameMap
--      b <- M.lookup nameB nameMap
--      modifyIORef fieldRef (\graph -> graph {
--        edges = (a,b) `S.insert` (edges graph) })
--  postRedisplay Nothing

--unrelated graph a b = not (a == b || (a,b) `S.member` edges graph || (b,a) `S.member` edges graph)

addRandom fieldRef label = do
  [w1,w2,w3] <- replicateM 3 $ randomRIO (-50,50)
  --[v1,v2] <- replicateM 2 $ randomRIO (-50,50)
  let v1 = 0; v2 = 0
  radius <- randomRIO (2,8) 
  node <- newNode label (w1,w2,0) (v1,v2,0) radius
  --modifyIORef nameMapRef $ M.insert name node
  modifyIORef fieldRef (\graph -> graph {
    nodes = node `S.insert` (nodes graph) })

addSpecial fieldRef label radius = do
  node <- newNode label (0,0,0) (0,0,0) radius
  modifyIORef fieldRef (\graph -> graph {
    nodes = node `S.insert` (nodes graph) })
  return node
  

waitMs :: Num a => a
waitMs = 10

boundary = 300

tick op = do
  op
  addTimerCallback waitMs (tick op)

animate fieldRef reports posRef self weapon = tick $ do
  writeIORef reports []
  
  graph <- readIORef fieldRef
  forM_ ((delete [] . tails . S.elems . nodes) graph) $ \(node:others) -> do
    current <- readIORef (position node)
    forM_ others $ \other -> do
      otherP <- readIORef (position other)
      let delta = subtract3 current otherP
      when (magnitude3 delta <= (radius other + radius node) && S.fromList [node,other] /= S.fromList [self,weapon]) $ do
        let angle = atan23 delta
        velDiff <- liftM2 subtract3 (readIORef $ velocity other) 
                                    (readIORef $ velocity node)
        let velAlign@(toward,perp,z) = rotate3 (-angle) velDiff
        --print $ show (floor (angle / pi * 180), toward)
        when (toward > 0) $ do
          curMomentum <- totalMomentum graph
          let impulseMag = 2 * toward / (recip (mass node) + recip (mass other))
          
          let impulse = rotate3 angle (- impulseMag, 0, 0)
          modifyIORef (velocity node) $ add3 $ mul3 (recip (mass node)) $ impulse
          modifyIORef (velocity other) $ add3 $ mul3 (recip (mass other)) $ subtract3 impulse (0,0,0)
          newMomentum <- totalMomentum graph
          let err = (magnitude3 (subtract3 curMomentum newMomentum)) in
            when (err > 0.001) $ do
              print (velAlign, curMomentum, newMomentum)
          modifyIORef reports $ (:) $ Report current [
            ((0,0,0),velDiff, 0,0,1),
            (rotate3 angle (radius node,0,0),rotate3 angle (radius node,-99999,0), 1,0,0),
            (rotate3 angle (radius node,0,0),rotate3 angle (radius node,99999,0), 1,1,1),
            ((0,0,0),rotate3 angle (toward,perp,0), 1,0,0),
            ((0,0,0),rotate3 angle (toward,0,0), if toward > 0 then 1 else 0,1,0),
            ((0,0,0),rotate3 angle (0,perp,0), if toward > 0 then 1 else 0,1,0)
              ]
      
      return ()
      -- ...
    
    -- collide with walls
    let (x,y,_) = current
        r = radius node
        constrain coord limit editor sign =
          when (coord + r > limit) $
            modifyIORef (velocity node) (editor (sign . abs))
      in do constrain x boundary first3 negate
            constrain y boundary second3 negate
            constrain (-x) boundary first3 id
            constrain (-y) boundary second3 id
    
    -- damping
    when (node /= weapon) $ do
      modifyIORef (velocity node) (mul3 0.99)
    
    
    -- update position
    v <- readIORef (velocity node)
    modifyIORef (position node) $ add3 (mul3 (waitMs / 1000) v)

  mp <- readIORef posRef
  sp <- readIORef (position self)
  wp <- readIORef (position weapon)
  modifyIORef (velocity self) $ (mul3 0.9 . add3 (mul3 0.9 $ subtract3 sp mp))
  modifyIORef (velocity weapon) $ (add3 (mul3 0.2 $ subtract3 wp mp))

  postRedisplay Nothing

handleMouse mousePosRef (Position x y) = do
  let conv c = fromIntegral (c - 400) * (boundary / 400)
  writeIORef mousePosRef (conv x,-conv y,0)

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= ([ DoubleBuffered, RGBMode ])
   initialWindowSize $= ((Size 800 800))
   initialWindowPosition $= ((Position 100 100))
   createWindow progName
   myInit
   
   fieldRef <- newIORef $ Graph (S.empty)
   mapM_ (addRandom fieldRef . show) [1..100]
   
   self <- addSpecial fieldRef "<>" 2
   weapon <- addSpecial fieldRef "." 10
   
   reports <- newIORef $ []
   
   view <- newIORef 0
   posRef <- newIORef (0,0,0)
   
   --passiveMotionCallback $= (Just $ \(Position x y) -> writeIORef view (fromIntegral x))
   passiveMotionCallback $= Just (handleMouse posRef)
   
   displayCallback $= (display fieldRef view reports)
   reshapeCallback $= (Just reshape)
   nameMapRef <- newIORef M.empty
   animate fieldRef reports posRef self weapon
   
   mainLoop