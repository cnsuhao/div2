import os
import sys

path = "/home/webmanic"
if path not in sys.path:
    sys.path.insert(0,path)

os.environ['DJANGO_SETTINGS_MODULE'] = 'webmanic.settings'

from django.core.handlers.wsgi import WSGIHandler
application = WSGIHandler()
