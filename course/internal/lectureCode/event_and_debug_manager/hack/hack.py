#!/usr/bin/python

import pygame
from pygame.locals import *

import math, os, sys
from random import random,randint

from subpixelsurface import SubPixelSurface

# our screen
if not pygame.font: print 'Warning, no fonts'
if not pygame.mixer: print 'Warning, no sound'
pygame.init()
screen = pygame.display.set_mode((1024,768))
pygame.display.set_caption('Shape Based Battles')
pygame.mouse.set_visible(False)

bounds = pygame.Rect(0,50,1024,718)

# our sprite groups
playersprites = pygame.sprite.RenderPlain()
baddies = pygame.sprite.RenderPlain()
bullets = pygame.sprite.RenderPlain()
effects = pygame.sprite.RenderPlain()
hud = pygame.sprite.RenderPlain()


def load_image(name, colorkey=None):
    fullname = os.path.join('data', name)
    try:
        image = pygame.image.load(fullname)
    except pygame.error, message:
        print 'Cannot load image:', name
        raise SystemExit, message
    image = image.convert()
    if colorkey is not None:
        if colorkey is -1:
            colorkey = image.get_at((0,0))
        image.set_colorkey(colorkey, RLEACCEL)
    return image, image.get_rect()
    
def load_sound(name):
    class NoneSound:
        def play(self): pass
    if not pygame.mixer:
        return NoneSound()
    fullname = os.path.join('data', name)
    try:
        sound = pygame.mixer.Sound(fullname)
    except pygame.error, message:
        print 'Cannot load sound:', wav
        raise SystemExit, message
    return sound



class BasicBaddie(pygame.sprite.Sprite):
	def __init__(self, x, y, dx, dy):
		super(BasicBaddie,self).__init__()
		self.image = pygame.Surface((30,30))
		self.image.fill((0,0,255))
		self.rect = self.image.get_rect()
		self.buff = SubPixelSurface(self.image)
		
		self.x, self.y, self.dx, self.dy = x, y, dx, dy
		self.rect.topleft = (x, y)
		
	
	def update(self):
		self.x += self.dx
		self.y += self.dy
		
		self.rect.topleft = (self.x, self.y)
		bound = self.rect.clamp(bounds)
		
		if bound.left != self.rect.left:
			self.dx = -self.dx
			self.x = bound.left
		if bound.top != self.rect.top:
			self.dy = -self.dy
			self.y = bound.top
			
		self.rect = bound

		self.image = self.buff.at(self.x,self.y)
		
	def die(self):
		self.kill()

	@classmethod
	def spawn(cls):
		to_close = True
		x,y = -1,-1
		
		while True:
			x,y = randint(0,screen.get_width()), randint(0,screen.get_height())
			
			clear = True
			for player in playersprites.sprites():
				clear = clear and (abs(player.rect.centerx-x)>60 or abs(player.rect.centery-y)>60)
			if clear: break	
			
		
		speed = 3
		angle = 2*math.pi*random()
		vx,vy = speed*math.sin(angle), speed*math.cos(angle)
		baddies.add(BasicBaddie(x,y,vx,vy))


class Explosion(pygame.sprite.Sprite):
	def __init__(self,centerx,centery,size=30):
		super(Explosion,self).__init__()


class Reticule(pygame.sprite.Sprite):
	def __init__(self):
		super(Reticule,self).__init__()
		self.image = pygame.Surface((20,20),SRCALPHA)
		self.rect = self.image.get_rect()
		self.image.fill((0,0,0,0))
		pygame.draw.circle(self.image,(0,0,0), (10,10), 10, 3)
		
		
	def update(self):
		#self.image = pygame.transform.rotate(self.image,math.radians(2))
		self.rect.center = pygame.mouse.get_pos()


class Player(pygame.sprite.Sprite):
	def __init__(self):
		super(Player,self).__init__()
		self.image = pygame.Surface((40,40))
		self.rect = self.image.get_rect()
		self.image.fill((255,255,255))
		pygame.draw.circle(self.image, (255,0,0), (20,20), 20)
		self.buff = SubPixelSurface(self.image)
		
		self.firerate    = 7
		self.timer       = 0
		self.bulletspeed = 12

	def update(self):
		x,y = self.rect.topleft
		self.image = self.buff.at(x,y)
		
	def fire(self):
		self.timer -= 1
		if self.timer>0:
			return
		self.timer = self.firerate
		
		# get the x/y velocities by normalizing 
		mx,my = pygame.mouse.get_pos()
		px,py = self.rect.center
		mx -= px
		my -= py
		r = math.sqrt((mx*mx)+(my*my))
		v = self.bulletspeed
		vx,vy = v*mx/r, v*my/r
		
		
		# get starting point
		x, y = self.rect.center
		dx, dy = 21*mx/r, 21*my/r
		x += dx
		y += dy
		
		bullets.add(Bullet(x,y,vx,vy))


	
class Bullet(pygame.sprite.Sprite):
	def __init__(self, x, y, dx, dy):
		super(Bullet,self).__init__()
		self.x, self.y, self.dx, self.dy = x, y, dx, dy
		
		self.image = pygame.Surface((10,10))
		self.rect = pygame.Rect(x-5,y-5,10,10)
		self.image.fill((0,0,0))
		self.buff = SubPixelSurface(self.image)
		
	def update(self):
		self.x += self.dx
		self.y += self.dy
		self.rect.center = (self.x, self.y)
		
		# destroy bullet if if is off the screen
		if not self.rect.colliderect(bounds):
			self.kill()
		
		self.image = self.buff.at(self.x, self.y)

			


def run():
	player = Player()
	playersprites.add(player)
	hud.add(Reticule())
	alive = True
	clock = pygame.time.Clock()
	c=0
	buff = pygame.Surface((screen.get_width(), screen.get_height()))

	# our main loop
	while True:
		clock.tick(60)
		c+=1
		
		# spawn new baddies ever 180 loops
		if c==180 and len(baddies.sprites())<40:
			c=0
			for i in range(3):
				BasicBaddie.spawn()

		############### INPUT ##################
		# handle events and single trigger buttons
		pressed = pygame.key.get_pressed()
		for event in pygame.event.get():
			if event.type == QUIT :
				sys.exit()
			elif event.type == KEYDOWN:
				if event.key == K_ESCAPE:
					sys.exit()
				if event.key == K_q and pressed[K_LCTRL]:
					sys.exit()

		# movement
		direction = [0,0]
		if alive:
			if pressed[K_a]:
				direction[0] += -1
			if pressed[K_d]:
				direction[0] += 1
			
			if pressed[K_w]:
				direction[1] += -1
			if pressed[K_s]:
				direction[1] += 1
				
		firing = any(pygame.mouse.get_pressed()) and alive
		
		############### UPDATE ######################
		# set player speed
		movement = 4.0
		if firing:
			movement = 3.0
		
		# if moving diagonally, halve the speed in both directions
		if all(direction):
			movement *= 1/math.sqrt(2)
			
		player.rect = player.rect.move((movement*direction[0], movement*direction[1]))
		player.rect = player.rect.clamp(bounds)  # keep player on screen
		
		# fire bullet if mouse is down, otherwise reset firerate timer
		if firing:
			player.fire()
		else:
			player.timer = 0
		
		# update sprites and handle collisions
		bullets.update()
		baddies.update()
		hud.update()
		
		hits = pygame.sprite.groupcollide(baddies, bullets, False, True) # remove both baddies and bullets that get hit
		for baddie in hits:
			baddie.die()
		
		# if player is hit, remove player and set to dead
		colls = pygame.sprite.groupcollide(baddies, playersprites, True, False)
		if len(colls):
			alive = False
			player.kill()
			hud.remove(hud.sprites())
		
		############### DISPLAY #####################
		# double buffer
		
		buff.blit(screen,(0,0))
		
		# bg
		buff.fill((255,255,255))
		
		#sprites
		playersprites.draw(buff)
		bullets.draw(buff)
		baddies.draw(buff)
		hud.draw(buff)
		
		#draw
		screen.blit(buff,(0,0))
		pygame.display.flip()


if __name__=='__main__':
	run()
