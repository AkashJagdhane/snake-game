snake-game using opengl in Haskell
==========

** About Game :
  
This is a Snake Game where a BLIND YELLOW COMPUTER SNAKE is always hungry and runs all over the screen to swallow anything that comes in its ways, most of the times, YELLOW SQUARE BOXES (assuming its favorite food is inside each box). Whenever Snake eats 1 box, its length increases. 
  Ofcourse, since snake is blind, player have to help the snake in providing proper directions so that snake will eat maximum boxes. 
  The games finishes when Snake swallows itself(i.e. when player gives wrong directions to the snake).

** How To install :

1) go to the extracted directory "snake-game-1.1"

2) on terminal , fire the following commands :
      $ sudo runhaskell Setup.hs configure
      $ sudo runhaskell Setup.hs build
      $ sudo runhaskell Setup.hs install

3) to check if the package is installed into ghci, do the following :
      $ ghci
      prelude> :module SnakeGame

   If you will reach something like "prelude SnakeGame>" in haskell then you can say that the package is properly installed.

4) well, game is ready to play now :

** How to play :

1) we have three modules : 
		SnakeGame.Game
		SnakeGame.Idle
		SnakeGame.Keyboard

2) last 2 modules are supporting modules where our concern is SnakeGame.Game. So we will load this module.

3) Prelude> :module SnakeGame.Game 
   Prelude SnakeGame.Game> startGame  

4) A game will start with a fullscreen. By pressing any of the four arrow keys, snake will start running. By using arrow keys player will have to give the directions to the snake.

  Note : Since snake is always hungry, it will be running always on the screen in the search of food.
