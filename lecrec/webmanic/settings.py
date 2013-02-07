DEBUG = True
TEMPLATE_DEBUG = DEBUG

import sys, os

ADMINS = (
    ('David Warshow', 'diw08@hampshire.edu'),
    ('Colin Roache', 'ccr09@hampshire.edu'),
    ('Alec Goebel', 'acg10@hampshire.edu'),
)

MANAGERS = ADMINS

DATABASE_ENGINE = 'mysql'
DATABASE_NAME = 'manic'
DATABASE_USER = 'manicAdmin'
DATABASE_PASSWORD = 'LCMFwbptzSqMcK7q'
DATABASE_HOST = 'localhost'
DATABASE_PORT = ''

CACHE_BACKEND = "memcached://127.0.0.1:11211/"

TIME_ZONE = 'America/New_York'
LANGUAGE_CODE = 'en-us'
SITE_ID = 1

# If you set this to False, Django will make some optimizations so as not
# to load the internationalization machinery.
USE_I18N = False

MEDIA_ROOT = '/home/webmanic/public_html/'
MEDIA_URL = '/static/'
ADMIN_MEDIA_PREFIX = '/static/admin/'

# Make this unique, and don't share it with anybody.
SECRET_KEY = '&y5ziqz294drcw3anh)0*wbw4htalk+ubhsx+603-!&+(0j_9('

# List of callables that know how to import templates from various sources.
TEMPLATE_LOADERS = (
    'django.template.loaders.filesystem.load_template_source',
    'django.template.loaders.app_directories.load_template_source',
#     'django.template.loaders.eggs.load_template_source',
)

MIDDLEWARE_CLASSES = (
    'django.middleware.common.CommonMiddleware',
    'django.contrib.sessions.middleware.SessionMiddleware',
    'django.contrib.auth.middleware.AuthenticationMiddleware',
)

ROOT_URLCONF = 'webmanic.urls'

TEMPLATE_DIRS = (
    "/home/webmanic/webmanic/templates/",
)

INSTALLED_APPS = (
    'django.contrib.auth',
    'django.contrib.contenttypes',
    'django.contrib.sessions',
    'django.contrib.sites',
    'django.contrib.admin',
#    'django.contrib.admindocs',

    'webmanic.courses',
    'webmanic.pres',
)

#
PRES_MEDIA_ROOT = "/stor4/media"
PRES_MEDIA_URL = "/media"
