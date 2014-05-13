module Game (startGame) where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Keyboard
import System.Random

displayPoints :: [(GLfloat,GLfloat,GLfloat)] -> PrimitiveMode -> IO ()
displayPoints points primitiveShape = do  
  renderAs primitiveShape points
  flush

renderAs :: PrimitiveMode -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderAs figure ps = renderPrimitive figure $ makeVertexes ps  

makeVertexes :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
makeVertexes = mapM_ (\ (x,y,z) -> vertex $ Vertex3 x y z)

_STEP = 0.0001
        
_NEXT = [(read "0.0"::GLfloat,read "0.0"::GLfloat,read "0.0"::GLfloat)]

_STEP1 = 98

startGame :: IO ()
startGame = do
  (progName,_) <- getArgsAndInitialize
  createWindow progName
  radius <- newIORef [(read "0.0"::GLfloat,read "0.0"::GLfloat,read "0.0"::GLfloat)]
  fullScreen
  score <- newIORef 0
  next <- newIORef _NEXT
  step1 <- newIORef _STEP1
  point <- newIORef (read "0.9000000000"::GLfloat,read "0.900000000"::GLfloat,read "0.0"::GLfloat)
  displayCallback $= display radius point
  keyboardMouseCallback $= Just (keyboard radius next step1 point score)
  mainLoop
    
fun :: [(GLfloat, GLfloat, GLfloat)] -> IO ()    
fun r = displayPoints r Points  
  
display :: (HasGetter g, HasGetter g1) => g [(GLfloat, GLfloat, GLfloat)] -> g1 (GLfloat, GLfloat, GLfloat) -> IO ()          
display radius point = do  
  clear [ColorBuffer]
  pointSize $= 60
  currentColor $= Color4 1 1 0 1
  r <- get radius
  p <- get point
  displayPoints (r ++ [p]) Points
  flush
