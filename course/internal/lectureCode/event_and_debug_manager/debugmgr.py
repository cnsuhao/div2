import pygame

DBG_FONT, DBG_SIZE,  DBG_LINEPADDING, = 'monospace', 14,  3
DBG_FG, DBG_BG, DBG_WIDTH , DBG_POS = (0,0,0), (128,128,128), 300, (0,0)

class DebugDisplayManager(dict):
	def __init__(self, *args):
		super(DebugDisplayManager, self).__init__(args)
		self.font = pygame.font.SysFont(DBG_FONT, DBG_SIZE)
		self.lineheight = self.font.size("TestHeight")[1] + DBG_LINEPADDING

	def render(self):
		renders = []
		max_length = 0
		for i,k in enumerate(self.keys()):
			string = k+": "+self[k]
			size   = self.font.size(string) 
			if max_length < size[0]:
				max_length = size[0]
			renders.append(self.font.render(string, True, DBG_FG, DBG_BG))

		surf = pygame.Surface((max_length+DBG_LINEPADDING*2, self.lineheight*len(self)))
		surf.fill(DBG_BG)
		for i,render in enumerate(renders):
			surf.blit(render,(DBG_LINEPADDING, i*self.lineheight))
		return surf
		
