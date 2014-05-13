module Idle(idleUp,idleDown,idleLeft,idleRight,makeGLfloat) where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import System.Random

makeExit :: (GLfloat,GLfloat,GLfloat) -> [(GLfloat,GLfloat,GLfloat)] -> Bool
makeExit pt lst = or $ map (makePts pt) lst

          
finalFunPts :: [(GLfloat,GLfloat,GLfloat)] 
               -> [(GLfloat,GLfloat,GLfloat)] 
               -> [(GLfloat,GLfloat,GLfloat)]
finalFunPts [] [] = []
finalFunPts [] lst2 = []
finalFunPts lst1 [] = []
finalFunPts (e1:lst1) (e2:lst2) = (funPts e1 e2) : (finalFunPts lst1 lst2)

makePts :: (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat) -> Bool
makePts (x1,y1,z1) (x2,y2,z2) = if(((read x::GLfloat) <= 0.001) && ((read y::GLfloat) <= 0.001)) then True else False
  where
    x = if(head xl == '-') then (tail xl) else xl
    y = if(head yl == '-') then (tail yl) else yl
    xl = number (x1 - x2)   
    yl = number (y1 - y2)
      
number :: GLfloat -> String
number n = if(n < 0) then ("-" ++ (number (-n))) else(if(elem 'e' a) then f else (show n))
  where
    a = show n
    n1 = takeWhile (/='e') a           
    n2 = read (drop 1 $ dropWhile (/='e') a) :: Int
    f = if(n2 < 0) then ((take len pos) ++ "." ++ (drop len pos)) else ((take (len+n2) neg) ++ "." ++ (drop (len+n2) neg)) 
    pos = ((take (-n2) (repeat '0')) ++ (filter (/='.') n1)) 
    neg = (filter (/='.') n1) ++ (take ((2 * n2) + n2) (repeat '0')) 
    len = length $ takeWhile (/='.') n1
    
funPts :: (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat) 
funPts (x1,y1,z1) (x2,y2,z2) = if(x1 == x2) then (if(y1>y2) then (x1,y1-0.00001,z1) else (x1,y1+0.00001,z1)) else (if(y1 == y2) then (if(x1 > x2) then (x1-0.00001,y1,z1) else (x1+0.00001,y1,z1)) else(x1,y1,z1))

makeGLfloat :: (Show a, Show a1, Show a2) => (a, a1, a2) -> (GLfloat, GLfloat, GLfloat)
makeGLfloat (x,y,z) = (read (show x)::GLfloat,read (show y)::GLfloat,read (show z)::GLfloat)

idleUp
  :: (Num a, Num a1, Ord a1, Show a, HasSetter s2, HasSetter s,
      HasSetter s1, HasSetter s3, HasSetter s4, HasGetter s2,
      HasGetter s4, HasGetter s3, HasGetter s1, HasGetter s) =>
     s3 [(GLfloat, GLfloat, GLfloat)]
     -> s2 [(GLfloat, GLfloat, GLfloat)]
     -> s4 a1
     -> s1 (GLfloat, GLfloat, GLfloat)
     -> s a
     -> IO ()

idleUp radius next step1 point score = do
  r <- get next
  s1 <- get step1
  let (a,b,c) = head r
  let [x,y,z,t] = r        
  if(b>(1.0)) then next $= ([(a, read "-0.9"::GLfloat, c)] ++ (take ((length r)-1) r))  
              else return ()
  
  r <- get radius
  rr <- get next
  let w = head r
  let l = head rr    
  let (a,b,c) = l    
  aa <- get point    
  let (px,py,pz) = aa
  ss <- get score
  let (rx,ry,rz) = last r
  let (nx,ny,nz) = last rr    
  if(makeExit w (tail r)) then do putStrLn ("score is:" ++ (show ss))
                                  leaveMainLoop
                          else return()
    
  if(makePts w aa) then do score $= ss + 1
                           let x = getStdRandom $ randomR (read (show (-0.9))::GLfloat,read (show 0.9)::GLfloat)
                           let y = getStdRandom $ randomR (read (show (-0.9))::GLfloat ,read (show 0.9)::GLfloat)
                           x1 <- x    
                           y1 <- y
                           let a = number ((read (show x1))::GLfloat)
                           let b = number ((read (show y1))::GLfloat)
                           let p = if(head a == '-') then (take 4 a) else (take 3 a)
                           let q = if(head b == '-') then (take 4 b) else (take 3 b)
                           point $= (read p::GLfloat,read q::GLfloat,0.0)
                           radius $= r ++ [(rx,ry-0.1,rz)]
                           next $= rr ++ [(nx,ny-0.1,nz)] 
                   else do return ()
                        
  if(s1 < 0) then do radius $= map makeGLfloat rr 
                     next $= map makeGLfloat ([(a,b+0.1,c)] ++ (take ((length rr)-1) rr))
                     step1 $= 98

             else do radius $= finalFunPts r rr
                     step1 $= s1 - 1
  postRedisplay Nothing      

idleDown
  :: (Num a, Num a1, Ord a1, Show a, HasSetter s2, HasSetter s,
      HasSetter s1, HasSetter s3, HasSetter s4, HasGetter s2,
      HasGetter s4, HasGetter s3, HasGetter s1, HasGetter s) =>
     s3 [(GLfloat, GLfloat, GLfloat)]
     -> s2 [(GLfloat, GLfloat, GLfloat)]
     -> s4 a1
     -> s1 (GLfloat, GLfloat, GLfloat)
     -> s a
     -> IO ()

idleDown radius next step1 point score = do
  r <- get next
  s1 <- get step1
  let (a,b,c) = head r
  let [x,y,z,t] = r    
  if(b<(-1.0)) then next $= ([(a,read "0.9"::GLfloat,c)]  ++ (take ((length r)-1) r)) 
              else return ()
  
  r <- get radius
  rr <- get next
  let w = head r    
  let l = head rr    
  let (a,b,c) = l    
  aa <- get point    
  let (px,py,pz) = aa
  ss <- get score
  let (rx,ry,rz) = last r
  let (nx,ny,nz) = last rr    
  if(makeExit w (tail r)) then do putStrLn ("score is:" ++ (show ss))
                                  leaveMainLoop
    else return()    
  if(makePts w aa) then do score $= ss + 1
                           let x = getStdRandom $ randomR (read (show (-0.9))::GLfloat,read (show 0.9)::GLfloat)
                           let y = getStdRandom $ randomR (read (show (-0.9))::GLfloat ,read (show 0.9)::GLfloat)
                           x1 <- x    
                           y1 <- y
                           let a = number ((read (show x1))::GLfloat)
                           let b = number ((read (show y1))::GLfloat)
                           let p = if(head a == '-') then (take 4 a) else (take 3 a)
                           let q = if(head b == '-') then (take 4 b) else (take 3 b)
                           point $= (read p::GLfloat,read q::GLfloat,0.0) 
                           radius $= r ++ [(rx,ry+0.1,rz)]
                           next $= rr ++ [(nx,ny+0.1,nz)] 
                   else do return ()
      
  if(s1 < 0) then do radius $= map makeGLfloat rr 
                     next $= map makeGLfloat ([(a,b-0.1,c)] ++ (take ((length rr)-1) rr))
                     step1 $= 98

                     else do radius $= finalFunPts r rr                         
                             step1 $= s1 - 1
  postRedisplay Nothing      
  
idleLeft
  :: (Num a, Num a1, Ord a1, Show a, HasSetter s2, HasSetter s,
      HasSetter s1, HasSetter s3, HasSetter s4, HasGetter s2,
      HasGetter s4, HasGetter s3, HasGetter s1, HasGetter s) =>
     s3 [(GLfloat, GLfloat, GLfloat)]
     -> s2 [(GLfloat, GLfloat, GLfloat)]
     -> s4 a1
     -> s1 (GLfloat, GLfloat, GLfloat)
     -> s a
     -> IO ()

idleLeft radius next step1 point score = do
  r <- get next
  s1 <- get step1
  let (a,b,c) = head r
  let [x,y,z,t] = r    
  if(a<(-1.0)) then next $= ([(read "0.9"::GLfloat,b,c)]   ++ (take ((length r)-1) r)) 
              else return ()
  
  r <- get radius
  rr <- get next
  let w = head r
  let l = head rr    
  let (a,b,c) = l    
  aa <- get point    
  let (px,py,pz) = aa
  ss <- get score
  let (rx,ry,rz) = last r
  let (nx,ny,nz) = last rr    
  if(makeExit w (tail r)) then do putStrLn ("score is:" ++ (show ss))
                                  leaveMainLoop
    else return()    
  if(makePts w aa) then do score $= ss + 1
                           let x = getStdRandom $ randomR (read (show (-0.9))::GLfloat,read (show 0.9)::GLfloat)
                           let y = getStdRandom $ randomR (read (show (-0.9))::GLfloat ,read (show 0.9)::GLfloat)
                           x1 <- x    
                           y1 <- y
                           let a = number ((read (show x1))::GLfloat)
                           let b = number ((read (show y1))::GLfloat)
                           let p = if(head a == '-') then (take 4 a) else (take 3 a)
                           let q = if(head b == '-') then (take 4 b) else (take 3 b)
                           point $= (read p::GLfloat,read q::GLfloat,0.0) 
                           radius $= r ++ [(rx+0.1,ry,rz)]
                           next $= rr ++ [(nx+0.1,ny,nz)] 
                   else do return ()
                   
  if(s1 < 0) then do radius $= map makeGLfloat rr 
                     next $= map makeGLfloat ([(a-0.1,b,c)]  ++ (take ((length rr)-1) rr))
                     step1 $= 98

                     else do radius $= finalFunPts r rr
                             step1 $= s1 - 1  
  postRedisplay Nothing        
  
idleRight
  :: (Num a, Num a1, Ord a1, Show a, HasSetter s2, HasSetter s,
      HasSetter s1, HasSetter s3, HasSetter s4, HasGetter s2,
      HasGetter s4, HasGetter s3, HasGetter s1, HasGetter s) =>
     s3 [(GLfloat, GLfloat, GLfloat)]
     -> s2 [(GLfloat, GLfloat, GLfloat)]
     -> s4 a1
     -> s1 (GLfloat, GLfloat, GLfloat)
     -> s a
     -> IO ()

idleRight radius next step1 point score = do
  r <- get next
  s1 <- get step1
  let (a,b,c) = head r
  let [x,y,z,t] = r    
  if(a>(1.0)) then next $= ([(read "-0.9"::GLfloat,b,c)]    ++ (take ((length r)-1) r)) 
              else return ()
  
  r <- get radius
  rr <- get next
  let w = head r
  let l = head rr    
  let (a,b,c) = l    
  aa <- get point    
  let (px,py,pz) = aa
  ss <- get score
  let (rx,ry,rz) = last r
  let (nx,ny,nz) = last rr    
  if(makeExit w (tail r)) then do putStrLn ("score is:" ++ (show ss))
                                  leaveMainLoop
    else return()    
  if(makePts w aa) then do score $= ss + 1                           
                           let x = getStdRandom $ randomR (read (show (-0.9))::GLfloat,read (show 0.9)::GLfloat)
                           let y = getStdRandom $ randomR (read (show (-0.9))::GLfloat ,read (show 0.9)::GLfloat)
                           x1 <- x    
                           y1 <- y
                           let a = number ((read (show x1))::GLfloat)
                           let b = number ((read (show y1))::GLfloat)
                           let p = if(head a == '-') then (take 4 a) else (take 3 a)
                           let q = if(head b == '-') then (take 4 b) else (take 3 b)
                           point $= (read p::GLfloat,read q::GLfloat,0.0) 
                           radius $= r ++ [(rx-0.1,ry,rz)]
                           next $= rr ++ [(nx-0.1,ny,nz)] 
                   else do return ()
                   
  if(s1 < 0) then do radius $= map makeGLfloat rr 
                     next $= map makeGLfloat ([(a+0.1,b,c)]   ++ (take ((length rr)-1) rr))
                     step1 $= 98

                     else do radius $= finalFunPts r rr
                             step1 $= s1 - 1
  postRedisplay Nothing        
