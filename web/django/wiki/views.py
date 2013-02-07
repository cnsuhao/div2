# Create your views here.

from django.http import Http404, HttpResponseRedirect
from django.shortcuts import render_to_response
from django.contrib.auth.views import redirect_to_login, login
from django.contrib.auth.decorators import login_required

from models import Page, Design, PageForm, UploadForm
from settings import *

from django.views.generic import list_detail, create_update
import os


@login_required	
def design_list(request):
	return list_detail.object_list(request,
		template_name="wiki/design_list.html",
		template_object_name='design',
		queryset=Design.objects.filter(owner=request.user))

@login_required	
def edit_design(request, design_slug):
	try:
		design=Design.objects.get(slug__iexact=design_slug)
	except Design.DoesNotExist:
		raise Http404	
	
	return create_update.update_object(request,
		form_class=UploadForm,
		object_id=design.id,
		post_save_redirect=WIKI_PREFIX_URL+'accounts/',	
		template_name="wiki/create_design.html",
		template_object_name='design')

@login_required	
def delete_design(request, design_slug):
	try:
		design=Design.objects.get(slug__iexact=design_slug)
	except Design.DoesNotExist:
		raise Http404	
		
	if request.user != design.owner:
		raise Http404	
		
	return create_update.delete_object(request,
		model=Design,
		post_delete_redirect=WIKI_PREFIX_URL+'accounts/',
		object_id = design.id,
		template_name="wiki/confirm_delete.html",
		template_object_name='design')

		
@login_required	
def create_design(request):
	if request.method == 'POST':
		form = UploadForm(request.POST, request.FILES)
		if form.is_valid():
			slug = form.cleaned_data['slug']
			form.save()
			design=Design.objects.get(slug__iexact=slug)
			design.owner = request.user
			design.save()
			return HttpResponseRedirect(WIKI_PREFIX_URL+'accounts/')
		else:
			return render_to_response('wiki/create_design.html', {'form':form})
	else:
		form = UploadForm()
		return render_to_response('wiki/create_design.html', {'form':form})

		

# NOTE, index and base MUST exist, or equivilent defaults
def show_page(request,page_slug=WIKI_DEFAULT_PAGE,design_slug=WIKI_DEFAULT_DESIGN):
	try:
		page=Page.objects.get(slug__iexact=page_slug)
	except Page.DoesNotExist:
		page=Page(slug=page_slug,title=page_slug,content="Click edit to change the page")
		page.save()
	try:
		design=Design.objects.get(slug__iexact=design_slug)
	except Design.DoesNotExist:
		raise Http404	
	
	template = os.path.join(design.path,'view.html')
			
	return render_to_response(template, {'page': page, 'design': design, 'media_url':WIKI_PREFIX_URL+'site_media/'+design_slug,'wiki_url':WIKI_PREFIX_URL})



def edit_page(request,page_slug=WIKI_DEFAULT_PAGE,design_slug=WIKI_DEFAULT_DESIGN):
	try:
		page=Page.objects.get(slug__iexact=page_slug)
	except Page.DoesNotExist:
		page=Page(slug=page_slug,title=page_slug,content="Click edit to change the page")
		page.save()
	try:
		design=Design.objects.get(slug__iexact=design_slug)
	except Design.DoesNotExist:
		raise Http404
	
	template = os.path.join(design.path,'edit.html')
	if design_slug != WIKI_DEFAULT_DESIGN:
		view_page = WIKI_PREFIX_URL+'%s/%s' % (design_slug,page_slug)
	else:
		view_page = WIKI_PREFIX_URL+'%s' % (page_slug)
	
	if not page.editable:
		return HttpResponseRedirect(view_page)
	
	return create_update.update_object(request,
		form_class=PageForm,
		object_id=page.id,
		post_save_redirect=view_page,	
		template_name=template,
		template_object_name='page',
		extra_context={'design': design, 'media_url':WIKI_PREFIX_URL+'site_media/'+design_slug,'wiki_url':WIKI_PREFIX_URL})

