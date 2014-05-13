module Keyboard (keyboard) where
import Idle
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Random

keyboard
  :: (Num a, Num a1, Ord a1, Show a, HasSetter s2, HasSetter s,
      HasSetter s1, HasSetter s3, HasSetter s4, HasGetter s3,
      HasGetter s2, HasGetter s4, HasGetter s1, HasGetter s) =>
     s3 [(GLfloat, GLfloat, GLfloat)]
     -> s2 [(GLfloat, GLfloat, GLfloat)]
     -> s4 a1
     -> s1 (GLfloat, GLfloat, GLfloat)
     -> s a
     -> Key
     -> KeyState
     -> t
     -> t1
     -> IO ()

keyboard radius next step1 point score (SpecialKey KeyUp) Down _ _ = do  
  r <- get radius  
  let (a,b,c) = head r
  next $= map makeGLfloat ([(a,b+0.1,c)] ++ (take ((length r)-1) r))  
  idleCallback $= Just (idleUp radius next step1 point score)        
  postRedisplay Nothing

keyboard radius next step1 point score (SpecialKey KeyDown) Down _ _ = do  
  r <- get radius  
  let (a,b,c) = head r
  next $= map makeGLfloat ([(a,b-0.1,c)] ++ (take ((length r)-1) r))    
  idleCallback $= Just (idleDown radius next step1 point score)        
  postRedisplay Nothing
  
keyboard radius next step1 point score (SpecialKey KeyLeft) Down _ _ = do  
  r <- get radius  
  let (a,b,c) = head r
  next $= map makeGLfloat ([(a-0.1,b,c)] ++ (take ((length r)-1) r))    
  idleCallback $= Just (idleLeft radius next step1 point score)        
  postRedisplay Nothing  
  
keyboard radius next step1 point score (SpecialKey KeyRight) Down _ _ = do  
  r <- get radius  
  let (a,b,c) = head r
  next $= map makeGLfloat ([(a+0.1,b,c)] ++ (take ((length r)-1) r))  
  idleCallback $= Just (idleRight radius next step1 point score)        
  postRedisplay Nothing  

keyboard _ _ _ _ _ _ _ _ _ = return ()  
