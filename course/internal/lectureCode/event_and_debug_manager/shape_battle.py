#!/usr/bin/python

import os,sys
import pygame
from pygame.locals import *

import numpy

from debugmgr import DebugDisplayManager, DBG_POS
from eventmgr import EventManager, QuitMessage

SCREEN_SIZE = SCREEN_W, SCREEN_H = 640, 480
BG_COLOR = 255,255,255

FRAME_RATE = 60

DEBUG = True

class GameManager():
	def __init__(self):
		pygame.init()
		self.screen = pygame.display.set_mode(SCREEN_SIZE)  # create a screen with no
		self.clock  = pygame.time.Clock()
		
		self.info  = DebugDisplayManager()
		self.event = EventManager()
	
		self.event.setChords('quit', (K_ESCAPE,), (K_LCTRL, K_q))
	
	def draw(self):
		# create buffer
		dblbuff = pygame.Surface(SCREEN_SIZE)
		
		# draw background
		dblbuff.fill(BG_COLOR)
		
		# debug
		if DEBUG:
			dblbuff.blit(self.info.render(), DBG_POS)
		
		#update display
		self.screen.blit(dblbuff,(0,0))
		pygame.display.flip()
	
	def tick(self):
		self.clock.tick(FRAME_RATE)
		self.info['FPS'] = "%f" % self.clock.get_fps()
		
		self.event.tick()

		self.draw()
		
	def quit(self):
		sys.exit()
	
	def run(self):
		while True:
			try:
				self.tick()

			except QuitMessage, e:
				print 'Quit Message recieved: ',e.value
				self.quit()
		
	

if __name__=='__main__':
	GameManager().run()

