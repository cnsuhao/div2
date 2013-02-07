from django.contrib import admin

from webmanic.manic.models.course import Course,Term
from webmanic.manic.admin.course import TermAdmin

admin.site.register(Course)
admin.site.register(Term, TermAdmin)
from django.contrib import admin

from webmanic.manic.models.course import Course,Term
from webmanic.manic.admin.course import TermAdmin

admin.site.register(Course)
admin.site.register(Term, TermAdmin)
