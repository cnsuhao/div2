"""
This module simply has several global variables necessary for the wiki.

	WIKI_UPLOAD_DIR:     where new zip files are saved
	WIKI_MEDIA_DIR:      where all css,js, and images go
	WIKI_TEMPLATES_DIR:  where new templates go
	
After setting each of these to a default value, it then checks to see if any of them are overridden
in django.conf.settings (the root settings.py).
"""

import os

WIKI_BASE_DIR = os.path.dirname(os.path.abspath(__file__))

WIKI_UPLOAD_DIR = os.path.join(WIKI_BASE_DIR, 'upload')
WIKI_MEDIA_DIR  = os.path.join(WIKI_BASE_DIR, 'media')
WIKI_DESIGN_DIR = os.path.join(WIKI_BASE_DIR, 'designs')

WIKI_PREFIX_URL = '/wiki/'
WIKI_DEFAULT_PAGE = 'index'
WIKI_DEFAULT_DESIGN = 'base'

# override defaults with set values
from django.conf import settings

if 'WIKI_UPLOAD_DIR' in dir(settings):
	WIKI_UPLOAD_DIR = settings.WIKI_UPLOAD_DIR 
if 'WIKI_MEDIA_DIR' in dir(settings):
	WIKI_MEDIA_DIR = settings.WIKI_MEDIA_DIR
if 'WIKI_DESIGN_DIR' in dir(settings):
	WIKI_DESIGN_DIR = settings.WIKI_DESIGN_DIR
