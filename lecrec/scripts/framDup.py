#!/usr/bin/python

import glob
import os
import re
from ConfigParser import ConfigParser

cfg = ConfigParser()
cfg.read("../INFO")
duration = cfg.getint("pres", "duration")

files = glob.glob("frame_*.ppm")

files.sort()

frames = {}

for frame in files:
    c, t = [ int(v) for v in re.findall(r"frame_(\d+)-(\d+).ppm", frame)[0]]

    if not frames.has_key(t):
        frames[t] = []
    frames[t].append([1,frame])


fps = max([len(frames[t]) for t in frames.keys()])

for grp in frames.values():
    l = len(grp)
    for i in range(fps-l):
        grp[i%l][0] += 1

start = int(re.findall(r"frame_\d+-(\d+).ppm", files[0])[0])


allFrames = [start + i for i in range(duration)]


### COMMENT HERE

for i, t in enumerate(allFrames):
    if frames.has_key(t):
        allFrames[i]=frames[t]
    else:
        allFrames[i] = [ [fps, allFrames[i-1][-1][1]] ]
    
try:
    os.mkdir("frames")
except:
    pass

n=1
for grp in allFrames:
    for c, frame in grp:
        for i in range(c):
            os.link(frame,"frames/frame_%08d.ppm" % n)
            n+=1
print fps



