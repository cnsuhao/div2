from django.contrib import admin

from models import Course,Term

class TermAdmin(admin.ModelAdmin):
    fieldsets = (
        (None, {
            "fields": ("termid", "name", ("start", "end")),
        }),
    )

class CourseAdmin(admin.ModelAdmin):
    list_display = ("courseid", "name", "term")
    list_filter = ("term",)

admin.site.register(Course, CourseAdmin)
admin.site.register(Term, TermAdmin)
