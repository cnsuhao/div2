import pygame
from pygame.locals import *
import pygame.key as Key
import pygame.event as Event

class QuitMessage(Exception):
	def __init__(self, value):
		self.value = value
	def __str__(self):
		return repr(self.value)


class EventManager():
	_pressed  = []
	_chordmap = {}
	_inputs   = {}

	def __init__(self):
		pass
	
	def tick(self):
		for event in Event.get():
			if event.type == pygame.QUIT: raise QuitMessage('close button')
		self._pressed = Key.get_pressed()
		if self._inputs.has_key('quit'):
			if self.isPressed('quit'):
				raise QuitMessage('a quit chord was pressed')
				
	def setChords(self, index, *key_chords):
		chords = []
		for i,chord in enumerate(key_chords):
			try:
				chord[0]
			except TypeError:
				print "TypeError: Key Chords must be a tuple or a list"
			else:
				k = index+('%d'%i)
				self._chordmap[k]=chord
				chords.append(k)
		self._inputs[index] = chords
		
	def isPressed(self, index):
		if not self._inputs.has_key(index):
			return False
			
		chords = []
		for i in self._inputs[index]:
			keys = []
			for k in self._chordmap[i]:
				keys.append(self._pressed[k])
			chords.append(keys)
		
                return any(map(all, chords))
