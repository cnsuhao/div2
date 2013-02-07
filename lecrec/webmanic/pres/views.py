# Create your views here.
from datetime import date

from django.shortcuts import render_to_response
from webmanic.courses.models import Course
from models import Presentation
#from django.views.decorators.cache import cache_page
from django.views.decorators.cache import never_cache

#@cache_page(60 * 60 * 24)
@never_cache
def presentation(request, termid, courseid, pres_date, idx):
    pres_date = [ int(i) for i in pres_date.split('-') ]
    pres_date = date(*pres_date)
    idx = int(idx) - 1

    course = Course.objects.get(term=termid, courseid=courseid)
    pres = Presentation.objects.filter(date=pres_date, course=course)[idx]
    duration = "%d:%02d" % (pres.duration/60, pres.duration%60)
    
    return render_to_response("presentation.html", locals())
