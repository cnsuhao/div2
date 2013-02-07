from django.conf.urls.defaults import *

from views import show_page, edit_page, design_list, delete_design, edit_design, create_design

urlpatterns = patterns('',
	(r'^$', show_page),

	(r'^site_media/(?P<path>.*)$', 'django.views.static.serve', {'document_root': '/www/apps/wiki/designs/'}),

	(r'^accounts/delete/(?P<design_slug>\w+)$', delete_design),
	(r'^accounts/create$', create_design),
	(r'^accounts/(?P<design_slug>\w+)$', edit_design),
	(r'^accounts/$', design_list),


	(r'^edit/(?P<page_slug>\w+)$', edit_page),
	(r'^(?P<page_slug>\w+)$', show_page),

	(r'^(?P<design_slug>\w+)/edit/(?P<page_slug>\w+)$', edit_page),	
	(r'^(?P<design_slug>\w+)/(?P<page_slug>\w+)$', show_page),
	
	#view, edit, account, upload
)

