# Create your views here.
from django.shortcuts import render_to_response
from models import Term, Course

def term(request, termid):
    term = Term.objects.get(termid=termid)
    return render_to_response('term.html', locals())

def course(request, termid, courseid):
#    term = Term.objects.get(termid=termid)
    course = Course.objects.get(term=termid, courseid=courseid)
    return render_to_response('course.html', locals())

def index(request):
    term = Term.objects.current()
    return render_to_response('index.html', locals())
