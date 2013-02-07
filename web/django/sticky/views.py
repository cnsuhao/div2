# Create your views here.
from django.core import serializers 

from django.http import HttpResponse

from models import *

def get_notes(request):
        print request.is_ajax()
	data = serializers.serialize("json", Note.objects.filter(trash=False))
	return HttpResponse(data, mimetype="application/javascript")
	
def save_note(request):
	key = int(request.POST[u'pk'])
	note = Note.objects.get(pk=key)
	note.x = int(request.POST[u'x'])
	note.y = int(request.POST[u'y'])
	note.w = int(request.POST[u'w'])
	note.h = int(request.POST[u'h'])
	note.text = request.POST[u'text']
	note.bring_to_top()
	note.save()
	
	return HttpResponse("")
	

def trash_note(request):	
	key = int(request.POST[u'pk'])
	note = Note.objects.get(pk=key)
	note.trash = True
	note.save()
	
	return HttpResponse("")

def get_trash(request):
	data = serializers.serialize("json", Note.objects.filter(trash=True))
	return HttpResponse(data, mimetype="application/javascript")
	

def restore_note(request):	
	key = int(request.GET[u'pk'])
	note = Note.objects.get(pk=key)
	note.trash = False
	note.save()
	
	note.bring_to_top()
	note.save()
	
	data = serializers.serialize("json", Note.objects.filter(pk=note.pk))
	return HttpResponse(data, mimetype="application/javascript")

def delete_note(request):
	key = int(request.POST[u'pk'])
	note = Note.objects.get(pk=key)
	note.delete()
	
	return HttpResponse("")
	
def empty_trash(request):
	notes = Note.objects.filter(trash=True)
	for n in notes:
		n.delete()

def new_note(request):
	note = Note()
	note.save()
	note.bring_to_top()
	note.save()
	
	data = serializers.serialize("json", Note.objects.filter(pk=note.pk))
	return HttpResponse(data, mimetype="application/javascript")
