from django.conf.urls.defaults import *

# Uncomment the next two lines to enable the admin:
from django.contrib import admin
admin.autodiscover()

urlpatterns = patterns('',
    # Example:
    # (r'^webmanic/', include('webmanic.foo.urls')),

    (r'^admin/doc/', include('django.contrib.admindocs.urls')),
    (r'^admin/', include(admin.site.urls)),
    
    (r'^(?P<termid>\w+)/(?P<courseid>\w+)/(?P<pres_date>[\d-]+)/(?P<idx>\d+)/$', 'webmanic.pres.views.presentation'),
    (r'^(?P<termid>\w+)/(?P<courseid>\w+)/(?P<pres_date>[\d-]+)/$', 'webmanic.pres.views.presentation', {'idx': '1'}),
    (r'^(?P<termid>\w+)/(?P<courseid>\w+)/$', 'webmanic.courses.views.course'),
    (r'^(?P<termid>\w+)/$','webmanic.courses.views.term'),
    (r'^$', 'webmanic.courses.views.index'),
)
