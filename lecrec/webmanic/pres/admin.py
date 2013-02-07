from django.contrib import admin

from models import *

class PresAdmin(admin.ModelAdmin):
    list_display = ('id', 'course', 'start')
    list_filter = ("course",)

admin.site.register(Presentation, PresAdmin)
admin.site.register(PresentationSource)

