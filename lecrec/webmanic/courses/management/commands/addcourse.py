from optparse import OptionParser

from django.core.management.base import BaseCommand, CommandError
from django.conf import settings

from webmanic.courses.models import Term, Course

##  OPTIONS
OPTS = OptionParser(option_list=BaseCommand.option_list, add_help_option=False)
OPTS.add_option("-n", "--name",
    dest = "name",
    default = None,
    help = "")


##  COMMAND
class Command(BaseCommand):
    option_list = OPTS.option_list

    def handle(self, termid, courseid, *args, **options):
        name = options.get("name")
        if name is None:
            name = courseid

        # create term
        term, created = Term.objects.get_or_create(termid=termid)
        if created:
            term.save()

        # create course
        course, created = Course.objects.get_or_create(courseid=courseid, term=term)
        course.name = name
        course.save()
