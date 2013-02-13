#!/usr/bin/python

import json
import os

from mercurial import ui, hg
from mercurial.node import hex

cwd = os.getcwd()
repo = hg.repository(ui.ui(), cwd)
ctx = repo.changectx('tip')

info = {
    "rev": ctx.rev(),
    "node": hex(ctx.node())
}

output = "(function (obj) { obj.info = %s; })(window.game);" % json.dumps(info)

with open(os.path.join(cwd, "public_html", "info.js"), 'w') as f:
    f.write(output)