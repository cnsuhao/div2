#!/usr/bin/python

import os

from glob import glob
from itertools import chain
from ConfigParser import ConfigParser

# handle css first
ROOT = os.path.join(os.getcwd(), "public_html")
css = os.path.join(ROOT, "css")
path = os.path.join(css, "all.css")

try:
    os.remove(path)
except:
    pass


os.chdir(css)
sheets = []
for root, dirs, files in os.walk('.'):
    paths = [ os.path.join(root, name)[2:] for name in files if name.endswith('.css') ]
    sheets += paths
    

lines = [ "@import url(%s);\n" % sheet for sheet in sheets ]

with open(path, 'w') as f:
    f.writelines(lines)

print "'%s' built successfully" % path


### BROKEN ###
# now handle javascript loading
"""
js = os.path.join(ROOT, "js")
input = os.path.join(js, "load.conf")
output = os.path.join(js, "load.js")
os.chdir(js)

config = ConfigParser()
with open(input, 'r') as f:
    config.readfp(f)
    
imports = [ (s, config.get(s, "import")) for s in config.sections() ]

code = ""
for sect, raw in imports:
    code += 'Importer.fetch("%s")\n' % sect

    lines = raw.strip().split("\n")
    for i, line in enumerate(lines):
        if not line.startswith('@'):
            lines[i] = glob(line)
        else:
            lines[i] = [line]

    imports = chain(*lines)
    
    stmts = []
    seen = []
    for stmt in imports:
        if stmt in seen:
            continue
        
        code += ""
        seen.append(stmt)
        
        if stmt.startswith('@'):
            stmts.append('\t.require("%s")' % stmt[1:])
        else:
            stmts.append('\t.add("js/%s")' % stmt.replace('\\','/'))
            
    code += '\n'.join(stmts) + ';'
    code += "\n\n"
    
with open(output, 'w') as f:
    f.write(code)
"""
    
#print "'%s' built successfully" % output