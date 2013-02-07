import os

from django.core.management.base import BaseCommand, CommandError
from django.conf import settings

from webmanic.pres.models import *

class Command(BaseCommand):
    def make_slideshow(self):
        return SlideshowSource()

    def make_av(self, path, typ):
        name = os.path.basename(path)

        src = AVSource()
        src.type = typ
        src.path,_ = os.path.splitext(name)
        return src

    def make_slides(self, src, path, sep):
        f = open(path)
        data = f.read().strip()
        f.close()

        slides = [ line.split(sep) for line in data.split('\n') ]

        for img, off in slides:
            img = img.strip()
            off = int(off)

            slide = Slide()
            slide.path = img
            slide.source = src
            slide.offset = off
            slide.save()

    def handle(self, presid, typ, path, sep=None, *args, **options):
        if typ == "slide":
            src = self.make_slideshow()
        elif typ == "audio":
            src = self.make_av(path, AV_TYPES.index('a'))
        elif typ == "video":
            src = self.make_av(path, AV_TYPES.index('v'))
        else:
            raise CommandError("Unknown source type '%s'"%typ)

        src.name = os.path.basename( os.path.dirname(path) )
        src.save()

        # link to presentation
        psrc = PresentationSource()
        psrc.pres = Presentation.objects.get(id=int(presid))
        psrc.source = src
        psrc.save()

        # create slides if necessary
        if typ == "slide":
            self.make_slides(src, path, sep)

        
