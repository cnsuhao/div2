from django.db import models

# Pages
class Page(models.Model):
    title   = models.CharField(max_length=30)
    slug    = models.SlugField(max_length=20, primary_key=True)
    content = models.TextField()
    url     = models.SlugField(max_length=50, default='', editable=False)

class PageMeta(models.Model):
    page    = models.OneToOneField(Page, primary_key=True)
    order   = models.PositiveIntegerField(editable=False)
    anchors = models.TextField(editable=False)


# Content
class ContentSlotField(models.SlugField):
    def __init__(self, *args, **kwargs):
            kwargs['max_length']=15
            kwargs['editable']=False
            kwargs['primary_key']=True
            super(ContentSlotField,self).__init__(*args, **kwargs)

class ContentPair(models.Model):
    content = models.TextField()
    slot    = ContentSlotField()


class Content(models.Model):
    slug = models.SlugField(max_length=30, editable=False, primary_key=True)
    display  = models.CharField(max_length=50)
    template = models.TextField()


# Style
class ColorField(models.CharField):
    def __init__(self, *args, **kwargs):
            kwargs['max_length']=15
            super(ColorField, self).__init__(*args, **kwargs)

class ColorSlotField(models.SlugField):
    def __init__(self, *args, **kwargs):
        kwargs['max_length']=15
        super(ColorSlotField, self).__init__(*args, **kwargs)

class ColorPalette(models.Model):
    slug    = models.SlugField(max_length=30, primary_key=True)
    display = models.CharField(max_length=50)

class ColorPair(models.Model):
    palette = models.ForeignKey(ColorPalette, editable=False)
    color   = ColorField()
    slot    = ColorSlotField()

# Menu
class Link(models.Model):
    pass

class MenuItem(models.Model):
    slug    = models.SlugField(max_length=15, primary_key=True)
    display = models.CharField(max_length=15)
    link    = models.ForeignKey(Link, null=True)
    parent  = models.ForeignKey('self',  null=True, blank=True)


# Home page
class Homepage(models.Model):
    greeting = models.CharField(max_length=40)
    description = models.TextField()

class Slide(models.Model):
    content = models.TextField()

