# class methods, decorators, and a factory method
class Enemy(Sprite):
    _objs = Group()
   
    @classmethod
    def all(cls):
        return cls._objs[:]

    @classmethod
    def spawn(cls):
        enemy = Enemy(3,4,5,6)
        cls._objs.append(enemy)

    def __init__(self, x, y, vx, vy):
        self.x = x
        self.y = y
        self.vx = vx
        self.vy = vy

# Quick Notes on Objects
#  * singletons
#     single instances
#     good with factories (create one, store it, return it each time you "init it")
# 
#  * Managers
#     handle complicated related objects
#         "DisplayManager"
#         "InputManager"
#     often use singletons

