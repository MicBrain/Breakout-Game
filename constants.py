								#################
								### CONSTANTS ###
								#################

"""
	"Breakout" is an arcade game developed and published by Atari, Inc.
It was conceptualized by Nolan Bushnell and Steve Bristow, influenced 
by the 1972 Atari arcade game Pong, and built by Steve Wozniak aided 
by Steve Jobs.

	In the game, a layer of bricks lines the top third of the screen. A 
ball travels across the screen, bouncing off the top and side walls of the 
screen. When a brick is hit, the ball bounces away and the brick is destroyed. 
The player loses a turn when the ball touches the bottom of the screen. To 
prevent this from happening, the player has a movable paddle to bounce the 
ball upward, keeping it in play. 

	This is the firt part of the game where you can find all Constant variables
used for the game. We choose those values without any specific purposes and most
of them are just a result of design preferences. You can check the alternative
design option just by changing their values. 

	Pygame module is the most important part of this project. Before running
the project make sure that you have installed that module. You can download
the module from this website: http://www.lfd.uci.edu/~gohlke/pythonlibs/#pygame
Installing the module is easy and does not require any specific knowledge.
"""

import sys
import math
import pygame

# Constant for determining the screen size. 

SIZE_OF_THE_SCREEN = 640, 480

# Dimensions of the Object

HEIGHT_OF_BRICK  = 15
WIDTH_OF_BRICK   = 60

HEIGH_OF_PADDLE = 12
PADDLE_WIDTH  = 60

BALL_DIAMETER = 16
BALL_RADIUS   = BALL_DIAMETER // 2

# X coordinate for Paddle
MAX_PADDLE_X = SIZE_OF_THE_SCREEN[0] - PADDLE_WIDTH

MAX_BALL_X   = SIZE_OF_THE_SCREEN[0] - BALL_DIAMETER
MAX_BALL_Y   = SIZE_OF_THE_SCREEN[1] - BALL_DIAMETER

# Y coordinate for Paddle
PADDLE_Y = SIZE_OF_THE_SCREEN[1] - HEIGH_OF_PADDLE - 10


# Color constants
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
BLUE  = (0, 0, 255)
COLOR_OF_BRICK = (255, 0, 0)

# State constants
STATE_BALL_IN_PADDLE = 0
STATE_PLAYING = 1
STATE_WON = 2
STATE_GAME_OVER = 3
