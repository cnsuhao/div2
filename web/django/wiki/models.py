from django.contrib.auth.models import User
from settings import *
from django.db import models

from django.forms import ModelForm

from datetime import datetime
import zipfile
import os, shutil

class Page(models.Model):
	"""
		A model which stores the data for a given wiki page.
	"""
	slug = models.SlugField("URL Slug")
	title = models.CharField("Page Title", max_length=50)
	content = models.TextField("Page Content",blank=True)
	last_updated = models.DateTimeField("Last Updated", editable=False)
	editable = models.BooleanField("Page can be editted by anyone", default=True)

	def save(self):
		self.last_updated = datetime.now()
		super(Page, self).save() 

	def __unicode__(self):
		return self.title

	class Meta:
		ordering=['title']

class PageForm(ModelForm):
	class Meta:
		model=Page
		fields=('title','content')
		
class Design(models.Model):
	"""
		A model to keep track of an uploaded design
	"""
	slug = models.SlugField("URL Slug", blank=True, unique=True)
	name = models.CharField("Design Name", max_length=50)
	archive = models.FileField("Zipfile", upload_to=WIKI_UPLOAD_DIR)
	path   = models.FilePathField(editable=False)
	owner = models.ForeignKey(User, null=True)
	request_public = models.BooleanField("User wants design to be public", default=False)
	public = models.BooleanField("Design is public", default=False)

	def base_template(self):
		return os.path.join(self.path,'base.html')

	def save(self):
		# not public if user doesn't want it to be
		if not self.request_public:
			self.public = False
		
		# remove old directory
		if os.path.isdir(self.path):
			shutil.rmtree(self.path)
		
		# build new ones
		self.path = os.path.join(WIKI_DESIGN_DIR, self.slug)
		os.mkdir(self.path)
		print self.path
		
		# unzip into appropriate folders
		zip = zipfile.ZipFile(self.archive)
		for name in zip.namelist():
			if name.endswith('/'):
				os.mkdir(os.path.join(self.path, name))
			else:
				f = open(os.path.join(self.path, name), 'wb')
				f.write(zip.read(name))
				f.close()
		
		super(Design, self).save() 
		
	def delete(self):
		# remove old directory
		os.remove(self.archive)
		if os.path.isdir(self.path):
			shutil.rmtree(self.path)
		super(Design, self).delete() 

	def __unicode__(self):
		return self.name

	class Meta:
		ordering = ['name']
		
class UploadForm(ModelForm):
	class Meta:
		model=Design
		fields=('slug','name','archive')
		
		
