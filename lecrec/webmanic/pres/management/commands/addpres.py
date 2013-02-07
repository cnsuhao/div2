import os

from ConfigParser import ConfigParser
from datetime import datetime
from optparse import OptionParser

from django.core.management.base import BaseCommand, CommandError
from django.conf import settings

from webmanic.courses.models import Term, Course
from webmanic.pres.models import Presentation

class Command(BaseCommand):

    def handle(self, path, *args, **options):
        path = os.path.abspath(path)
        root = os.path.dirname(path)
        media = getattr(settings, 'PRES_MEDIA_ROOT', "/")
        if media[-1] != "/":
            media += "/"
        root = root.replace(media, "")

        # load config
        config = ConfigParser()
        config.read(path)
        termid = config.get("course", "term")
        courseid = config.get("course", "id")
        duration = config.getint("pres", "duration")
        start_s = config.get("pres", "start").split(',')
        start = [ int(v) for v in start_s ]
        start = datetime(*start)

        # create term
        term, created = Term.objects.get_or_create(termid=termid)
        if created:
            term.save()

        # create course
        try:
            course = Course.objects.get(term=term, courseid=courseid)
        except Course.DoesNotExist:
            course = Course(courseid=courseid, term=term)
            course.name = courseid
            course.save()

        # create presentation
        try:
            pres = Presentation.objects.get(path=root)
        except Presentation.DoesNotExist:
            pres = Presentation()

        pres.course = course
        pres.start = start
        pres.duration = duration
        pres.path = root
        pres.save()

        print "Created Presentation: %d\n" % pres.id
        
