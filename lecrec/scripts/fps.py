#!/usr/bin/python

from ConfigParser import ConfigParser
import os
import sys

pres=sys.argv[1]

cfg=ConfigParser()
cfg.read(os.path.join(pres, "INFO"))

frames = len( os.listdir(os.path.join(pres, "LeftCam", "frames")) )
duration = float(cfg.getint("pres", "duration"))

print "%0.2f" % (frames/duration)
