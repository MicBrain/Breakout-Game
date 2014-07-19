							#####################
							### BREAKOUT GAME ###
							#####################

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

	This is the main part of the game. In order to play the game you should 
run this file. 

	Pygame module is the most important part of this project. Before running
the project make sure that you have installed that module. You can download
the module from this website: http://www.lfd.uci.edu/~gohlke/pythonlibs/#pygame
Installing the module is easy and does not require any specific knowledge.
"""

from constants import *

class Breakout:

    def __init__(self):
    	""" Introdcutory parameters. """
    	pygame.init() # Calling pygame module
    	self.screen = pygame.display.set_mode(SIZE_OF_THE_SCREEN)
    	pygame.display.set_caption(" BREAKOUT")
    	self.clock = pygame.time.Clock()
    	if pygame.font:
    		self.font = pygame.font.Font(None,30)
    	else:
    		self.font = None
    	self.init_game()
     
    def init_game(self):
    	""" Initialize the game. """
    	self.lives = 3
    	self.score = 0
    	self.state = STATE_BALL_IN_PADDLE
    	self.paddle   = pygame.Rect(300, PADDLE_Y,PADDLE_WIDTH, HEIGH_OF_PADDLE)
    	self.ball     = pygame.Rect(300,PADDLE_Y - BALL_DIAMETER,BALL_DIAMETER,BALL_DIAMETER)
    	self.ball_vel = [5,-5]
    	self.create_bricks()
        
    def create_bricks(self):
        y_ofs = 35
        self.bricks = []
        for i in range(7):
            x_ofs = 35
            for j in range(8):
                self.bricks.append(pygame.Rect(x_ofs,y_ofs,WIDTH_OF_BRICK,HEIGHT_OF_BRICK))
                x_ofs += WIDTH_OF_BRICK + 10
            y_ofs += HEIGHT_OF_BRICK + 5

    def draw_bricks(self):
        for brick in self.bricks:
            pygame.draw.rect(self.screen, COLOR_OF_BRICK, brick)
        
    def check_input(self):
        keys = pygame.key.get_pressed()
        if keys[pygame.K_LEFT]:
            self.paddle.left -= 5
            if self.paddle.left < 0:
                self.paddle.left = 0
        if keys[pygame.K_RIGHT]:
            self.paddle.left += 5
            if self.paddle.left > MAX_PADDLE_X:
                self.paddle.left = MAX_PADDLE_X
        if keys[pygame.K_SPACE] and self.state == STATE_BALL_IN_PADDLE:
            self.ball_vel = [5,-5]
            self.state = STATE_PLAYING
        elif keys[pygame.K_RETURN] and (self.state == STATE_GAME_OVER or self.state == STATE_WON):
            self.init_game()

    def move_ball(self):
        self.ball.left += self.ball_vel[0]
        self.ball.top  += self.ball_vel[1]
        if self.ball.left <= 0:
            self.ball.left = 0
            self.ball_vel[0] = -self.ball_vel[0]
        elif self.ball.left >= MAX_BALL_X:
            self.ball.left = MAX_BALL_X
            self.ball_vel[0] = -self.ball_vel[0]
        if self.ball.top < 0:
            self.ball.top = 0
            self.ball_vel[1] = -self.ball_vel[1]
        elif self.ball.top >= MAX_BALL_Y:            
            self.ball.top = MAX_BALL_Y
            self.ball_vel[1] = -self.ball_vel[1]

    def handle_collisions(self):
        for brick in self.bricks:
            if self.ball.colliderect(brick):
                self.score += 3
                self.ball_vel[1] = -self.ball_vel[1]
                self.bricks.remove(brick)
                break
        if len(self.bricks) == 0:
            self.state = STATE_WON    
        if self.ball.colliderect(self.paddle):
            self.ball.top = PADDLE_Y - BALL_DIAMETER
            self.ball_vel[1] = -self.ball_vel[1]
        elif self.ball.top > self.paddle.top:
            self.lives -= 1
            if self.lives > 0:
                self.state = STATE_BALL_IN_PADDLE
            else:
                self.state = STATE_GAME_OVER

    def show_stats(self):
        if self.font:
            font_surface = self.font.render("YOUR SCORE: " + str(self.score) + " LIVES: " + str(self.lives), False, WHITE)
            self.screen.blit(font_surface, (205,5))

    def show_message(self,message):
        if self.font:
            size = self.font.size(message)
            font_surface = self.font.render(message,False, WHITE)
            x = (SIZE_OF_THE_SCREEN[0] - size[0]) / 2
            y = (SIZE_OF_THE_SCREEN[1] - size[1]) / 2
            self.screen.blit(font_surface, (x,y))
              
    def run(self):
        while 1:            
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    sys.exit
            self.clock.tick(50)
            self.screen.fill(BLACK)
            self.check_input()
            if self.state == STATE_PLAYING:
                self.move_ball()
                self.handle_collisions()
            elif self.state == STATE_BALL_IN_PADDLE:
                self.ball.left = self.paddle.left + self.paddle.width / 2
                self.ball.top  = self.paddle.top - self.ball.height
                self.show_message("PRESS SPACE TO LAUNCH THE BALL")
            elif self.state == STATE_GAME_OVER:
                self.show_message("GAME OVER. PRESS ENTER TO PLAY AGAIN")
            elif self.state == STATE_WON:
                self.show_message("YOU WON! PRESS ENTER TO PLAY AGAIN")    
            self.draw_bricks()
            pygame.draw.rect(self.screen, BLUE, self.paddle)
            pygame.draw.circle(self.screen, WHITE, (self.ball.left + BALL_RADIUS, self.ball.top + BALL_RADIUS), BALL_RADIUS)
            self.show_stats()
            pygame.display.flip()

if __name__ == "__main__":
    Breakout().run()
