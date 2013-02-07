from django.contrib import admin
from models import Page, Design

class PageAdmin(admin.ModelAdmin):
	date_hierarchy = 'last_updated'
	list_display = ('title', 'slug','last_updated')
	list_filter = ['last_updated', 'editable']
	ordering = ['title']
	search_fields = ['title','slug']

class DesignAdmin(admin.ModelAdmin):
	list_display = ('name','slug','owner','request_public','public')
	list_filter = ['request_public','public']
	search_fields = ['name','slug','owner']

admin.site.register(Design, DesignAdmin)
admin.site.register(Page, PageAdmin)
