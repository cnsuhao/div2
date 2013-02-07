from django.conf.urls.defaults import *
from django.conf import settings
from views import *

urlpatterns = patterns('',
    (r'^all', get_notes),
    (r'^trashcan', get_trash),
    (r'^save', save_note),
    (r'^new', new_note),
    (r'^trash', trash_note),
    (r'^delete', delete_note),
    (r'^restore', restore_note),
    (r'^empty', empty_trash),
    (r'^$', 'django.views.generic.simple.direct_to_template', {'template': 'sticky/stickypage.html',
    				'extra_content': settings.MEDIA_URL} ),

)
