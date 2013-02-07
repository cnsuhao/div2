from datetime import datetime, timedelta

from django.db import models
from django.contrib.contenttypes.models import ContentType
from django.contrib.contenttypes import generic
from django.conf import settings

from webmanic.courses.models import Course

class PresentationSource(models.Model):
    pres = models.ForeignKey("Presentation", related_name="pressrc")
    content_type = models.ForeignKey(ContentType)
    object_id = models.PositiveIntegerField()
    source = generic.GenericForeignKey('content_type', 'object_id')

class PresentationManager(models.Manager):
    def filter(self, *args, **kwargs):
        if kwargs.has_key("date"):
            kwargs['start__gte'] = kwargs.get("date")
            kwargs['start__lt'] = kwargs.get("date") + timedelta(days=1)
            del kwargs['date']

        return super(PresentationManager, self).filter(*args, **kwargs)

class SourcesList(object):
    def __init__(self, sources):
        self._sources = {}
        for src in sources:
           self._sources[src.name] = src

    def __getattr__(self, name):
        return self._sources.get(name, None)

    def all(self):
        return self._sources.values()

    def keys(self):
        return self._sources.keys()


class Presentation(models.Model):
    course = models.ForeignKey(Course, related_name="pres")
    name = models.CharField(max_length=200, blank=True, default="")
    start = models.DateTimeField()
    duration = models.PositiveIntegerField()
    path = models.CharField(max_length=200, unique=True)

    objects = PresentationManager()

    date = property(lambda self: self.start.date())

    def date_index(self):
        if not hasattr(self, "_index"):
            pres = Presentation.objects.filter(date=self.start.date(), course=self.course)
            ids = [ p.id for p in pres ]
            self._index = ids.index(self.id) + 1
        return self._index

    def __unicode__(self):
        term = self.course.term.termid
        course = self.course.courseid
        return "%s %s %s #%02d (%d)" % (term, course, self.date.isoformat(), self.date_index(), self.id)

    def breadcrumbs(self):
        title = self.date.isoformat()
        if self.date_index() > 1:
            title += " %#02d" % self.date_index()
        return self.course.breadcrumbs() + [(title, self.get_absolute_url())]

    @property
    def title(self):
        title = self.date.strftime("%a %b %d, %Y")
        if self.date_index() > 1:
            title += " (%d)" % self.date_index()
        if self.name:
            title += ": %s" % self.name
        return title
        
    @property
    def sources(self):
        if not hasattr(self, "_sources_list"):
            self._sources_list = SourcesList([ s.source for s in self.pressrc.all()])
        return self._sources_list


    @property
    def slides(self):
        slideshow = ContentType.objects.get_for_model(SlideshowSource)
        shows = PresentationSource.objects.filter(content_type__pk=slideshow.id, pres=self)
        slides = [ slide for src in shows for slide in Slide.objects.filter(source=src.source) ]
        slides.sort(key=lambda s: s.offset)
        return slides

    def get_absolute_url(self):
        term = self.course.term.termid
        course = self.course.courseid
        
        url = "/%s/%s/%s/" % (term, course, self.date.isoformat())
        
        if self.date_index() > 1:
            url += "%d/" % self.date_index()
        return url

    class Meta:
        ordering = ["start", "id"]

class Source(models.Model):
    name = models.CharField(max_length=16)
    _pres = generic.GenericRelation(PresentationSource)

    @property
    def srctype(self):
        return self.__class__.__name__

    @property
    def pres(self):
        pres = self._pres.all()
        if len(pres) > 0:
            return pres[0].pres
        else:
            return None


    def __unicode__(self):
        return "%s %d" % (self.name, self.pres.id)

    class Meta:
        abstract = True

AV_TYPES = [
    'a',    # audio source type
    'v'     # video source type
]

class AVSource(Source):
    type = models.PositiveIntegerField(choices=enumerate(AV_TYPES))
    path = models.CharField(max_length=64)

    def get_absolute_url(self):
        parts = [
            settings.PRES_MEDIA_URL,
            self.pres.course.term.termid,
            self.pres.course.courseid,
            self.pres.start.strftime("%Y%m%d%H%M%S"),
            self.name,
            self.path ]
        return "/".join(parts)

class SlideshowSource(Source):
    pass

class Slide(models.Model):
    path = models.CharField(max_length=64)
    offset = models.IntegerField()
    source = models.ForeignKey(SlideshowSource, related_name="slides")

    @property
    def timestamp(self):
        return "%02d:%02d" % (self.offset/60, self.offset%60)

    def get_url(self, extra=None):
        parts = [
            settings.PRES_MEDIA_URL,
            self.source.pres.course.term.termid,
            self.source.pres.course.courseid,
            self.source.pres.start.strftime("%Y%m%d%H%M%S"),
            self.source.name ]

        if extra is not None:
            parts.append(extra)
        parts.append(self.path)
        return "/".join(parts)

    def get_absolute_url(self):
        return self.get_url()

    def get_thumb_url(self):
        return self.get_url("thumbs")

    def get_preview_url(self):
        return self.get_url("preview")


    class Meta:
            ordering = ('source', 'offset')


