from optparse import OptionParser

from django.core.management.base import BaseCommand, CommandError
from django.conf import settings

from webmanic.manic.models import Term, Course

##  OPTIONS
OPTS = OptionParser(option_list=BaseCommand.option_list, add_help_option=False)
OPTS.add_option("-c", "--cmd",
    dest = "cmd",
    default = getattr(settings, 'GENCRON_CMD', "cmd"),
    help = "")

OPTS.add_option("-f", "--format",
    dest = "format",
    default = getattr(settings, 'GENCRON_FORMAT', "${cmd} ${dur}"),
    help = "")

##  COMMAND
class Command(BaseCommand):
    option_list = OPTS.option_list

    def get_context(self):
        pass

    def handle(self, termid, *args, **options):
        self.stdout.write(str(self.option_list))
        self.stdout.write("\n")

        # fetch the term
        try:
            term = Term.objects.get(id=termid)
            self.stdout.write("Found term: %s" % term)
        except Term.DoesNotExist, ex:
            raise CommandError("Cannot find Term matching '%s'" % termid)

        self.stdout.write(cmd)
        self.stdout.write(" ")
        self.stdout.write("\n")

