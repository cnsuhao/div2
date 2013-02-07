from django.contrib import admin
from models import Note

class NoteAdmin(admin.ModelAdmin):
        search_fields = ['text']
        fieldsets = (
            (None, {'fields':('text',)}),
        )

admin.site.register(Note, NoteAdmin)     


