"""
rect.py

An attempt to recreate the properties of pygame's Rect objects.  Tried to recreate
it by adding new property types.  This was all in an attempt to create a "mini dsl"
so the object itself could be declared without any actual method/decorator syntax

"""
def attribute_prop(name, default=0):
    "basic attribute property, creates a hidden value for a given name"
    def func(self):
        return getattr(self, "_"+name, default)

    func.__name__ = name
    func = property(func)

    @func.setter
    def func(self,v):
        setattr(self, "_"+name, v)

    return func


def offset_prop(name, related, ratio):
    "offset the return value of variable `name` by `related * ratio`."

    @property
    def func(self):
        v = getattr(self, name)
        r = getattr(self, related)
        return v + r * ratio

    @func.setter
    def func(self, v):
        r = getattr(self, related)
        setattr(self, name, v - r * ratio)

    return func
        

def group_prop(*names):
    "return more than one property"

    def func(self):
        return [ getattr(self, n) for n in names ]

    func.__name__ = "_".join(names)
    func = property(func)

    @func.setter
    def func(self, vv):
        for n,v in zip(names, vv):
           setattr(self, n, v) 

    return func 

class Rect: 
    # geometry
    x = left = attribute_prop("x")
    y = top = attribute_prop("y")
    w = width = attribute_prop("w")
    h = height = attribute_prop("h")

    centerx = offset_prop("x", "w", 0.5)
    centery = offset_prop("y", "h", 0.5)
    right = offset_prop("x", "w", 1)
    bottom = offset_prop("y", "h", 1)

    topleft = group_prop("top", "left")
    bottomleft = group_prop("bottom", "left")
    topright = group_prop("top", "right")
    bottomright = group_prop("bottom", "right")
    midtop = group_prop("centerx", "top")
    midbottom = group_prop("centerx", "bottom")
    midleft = group_prop("left", "centery")
    midright = group_prop("right", "centery")
    center = group_prop("centerx", "centery")

    size = group_prop("w", "h")
    geom = group_prop("x", "y", "w", "h")
