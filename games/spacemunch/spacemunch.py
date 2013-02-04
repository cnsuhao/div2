import pygame, sys, random

screensize = scrW, scrH = 640, 480

""" Sprite Groups"""
bullets = pygame.sprite.RenderPlain()
bolts = pygame.sprite.RenderPlain()
powerups = pygame.sprite.RenderPlain()
baddies = pygame.sprite.RenderPlain()
deadbaddies = pygame.sprite.RenderPlain()
players = pygame.sprite.RenderPlain()

class Player(pygame.sprite.Sprite):
	def __init__(self, color):
		pygame.sprite.Sprite.__init__(self)
		self.image = pygame.Surface((40, 20))
		self.image.fill(color)
		self.rect = self.image.get_rect()

		self.x = scrW/2
		self.y = scrH*7/8

		self.dx = 5

		self.color = color

		self.firerate = 4
		self.count    = 0

		self.numbullets = 3

	def update(self):
		self.rect.midtop = self.x, self.y

	def moveLeft(self):
		self.x -= self.dx
		if self.x<=20:  self.x = 20
			
	def moveRight(self):
		self.x += self.dx
		if self.x>=620: self.x = 620

	def fireBullet(self):
		if self.numbullets%2 == 1: bullets.add(Bullet(self.x, self.y, self.color))

		n = self.numbullets/2

		if n==1:
				bullets.add(Bullet(self.x-(self.rect.width/2), self.y, self.color))
				bullets.add(Bullet(self.x+(self.rect.width/2), self.y, self.color))
		elif n==2:
				bullets.add(Bullet(self.x-(self.rect.width/2)/2, self.y, self.color, spread=-1.5))
				bullets.add(Bullet(self.x+(self.rect.width/2)/2, self.y, self.color, spread=1.5))
				bullets.add(Bullet(self.x-(self.rect.width/2), self.y, self.color, spread=-3))
				bullets.add(Bullet(self.x+(self.rect.width/2), self.y, self.color, spread=3))

                
# all shots are this class
class Bullet(pygame.sprite.Sprite):
	def __init__(self, x, y, color, speed=10, spread=0):
		pygame.sprite.Sprite.__init__(self)
		self.image = pygame.Surface((7, 10))
		self.image.fill(color)
		self.rect = self.image.get_rect()

		self.x = x
		self.y = y

		self.dy = speed
		self.dx = spread
			
	def update(self):
		self.y -= self.dy
		self.x += self.dx
		self.rect.midtop = self.x, self.y
		if self.y > scrH or self.y<0: self.kill()


# Our main baddie
class Munch(pygame.sprite.Sprite):
	def __init__(self, pos=None):
		pygame.sprite.Sprite.__init__(self)
		self.image = pygame.image.load("munch.bmp")
		self.image.set_colorkey(self.image.get_at((0,0)))
		self.rect = self.image.get_rect()

		self.dx = random.randint(-3,3)
		self.dy = random.randint(-3,3)

		if pos:
			self.x, self.y = pos
		else:
			self.x = random.randint(0,scrW-self.rect.width)
			self.y = random.randint(0,scrH*3/4-self.rect.height)

		if self.dx<0:
			self.image = pygame.transform.flip(self.image, True, False)

		self.dying = False
			
	def update(self):
		self.x += self.dx
		self.y += self.dy

		if self.x >= scrW-self.rect.width or self.x <= 0:
				self.dx = -self.dx
				self.image = pygame.transform.flip(self.image, True, False)
		if self.y >= scrH*3/4-self.rect.height or self.y <= 0:
				self.dy = -self.dy
		
		# if the number of baddies is less than 800, add more
		if len(baddies.sprites()) < 800 and random.randint(0,200)==0: baddies.add(Munch(pos=(self.x, self.y)))

		self.rect.topleft = self.x, self.y

		if random.randint(0,150) == 0 and self.y<320: self.createBolt()

	def kill(self):
		if random.randint(0,30)==0: self.createPowerup()
		pygame.sprite.Sprite.kill(self)

	def createPowerup(self):
		powerups.add(Bullet(self.x, self.y, (0,0,255), speed = -2))
	def createBolt(self):
		bolts.add(Bullet(self.x, self.y, (0,255,0), speed=-5))

			   
def run():
        pygame.init()

	#Make the window resolution 640 by 480, and give us the surface called screen
        screen = pygame.display.set_mode(screensize)

        player = Player((255,0,0))
        players.add(player)

        for i in range(50):
                baddies.add(Munch())
	
        clock = pygame.time.Clock()

	#Our game loop
        while True:
		clock.tick(50)  #limit fps to 50


		""" INPUT """
		for event in pygame.event.get():	#foreach through the events
			if event.type == pygame.QUIT:	#Special event for 'x' button
				sys.exit()
				
			if event.type == pygame.KEYDOWN and event.key == pygame.K_ESCAPE:
				sys.exit()
	
		#Handle Player Movement
		keys = pygame.key.get_pressed()
		
		if keys[pygame.K_LEFT]:	#If they're holding down left, move left, etc. 
			player.moveLeft()
		if keys[pygame.K_RIGHT]:
			player.moveRight()

		# handle gun
		if keys[pygame.K_SPACE]:
			if player.count == 0:
					player.count = player.firerate
					player.fireBullet()
			else:
					player.count -= 1


		""" Sprite Update"""
		baddies.update()
		players.update()
		bullets.update()
		powerups.update()
		bolts.update()

		""" Collision """
		baddiesHit = pygame.sprite.groupcollide(baddies, bullets, True, False)
		pickup     = pygame.sprite.groupcollide(players, powerups, False, True)
		playerHit  = pygame.sprite.groupcollide(players, bolts, False, True)

		for k in pickup:
			k.numbullets = k.numbullets%5+1


		for k in playerHit:
			k.numbullets -= 1
			if k.numbullets == 0:
					k.kill()

		"""Draw"""
		#Background
		screen.fill((255,255,255))
		pygame.draw.rect(screen, (0,0,0), (0,scrH*3/4,scrW,scrH*1/4), 0)

		# draw sprites
		players.draw(screen)
		baddies.draw(screen)
		bullets.draw(screen)
		powerups.draw(screen)
		bolts.draw(screen)

		# update screen
		pygame.display.flip()

# if this is actually the main function, run the game
if __name__ == '__main__':
	run()
