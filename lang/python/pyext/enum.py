#!/usr/bin/env python
import re

class ImmutableError(Exception):
    pass

class Enum(object):
    def __init__(self,*a):
        self.__dict__["__terms__"] = a
        for i,v in enumerate(a):
            self.__dict__[v] = i

    def keys(self):
        return self.__keys__

    def __setattr__(self,k,v):
        raise ImmutableError("Values in enumeration are final")

    def __repr__(self):
        return "<Enum: %s>" % ", ".join(self.__terms__)


def enum(s):
    kwreg = re.compile(r'\W?([A-Za-z_]\w*)\W?')
    comments = re.compile(r'#.*$', re.MULTILINE)

    if type(s) is not str:
        raise ValueError("You must pass a string of enum values.")
    kw = kwreg.findall( comments.sub('', s))
    return Enum(*kw)


if __name__ == "__main__":
    colors = enum("RED, YELLOW, BLUE")
    print colors
    print colors.RED, colors.YELLOW, colors.BLUE

    flags = enum("""
       no_space, # no space left in disk
       perm_error, # permissions on file open
    """)

    print flags
