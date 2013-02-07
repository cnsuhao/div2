from datetime import date

from django.db import models

class TermManager(models.Manager):
    def current(self):
        today = date.today()
        results = self.filter(start__lte=today, end__gte=today)
        if len(results) > 0:
            return results[0]
        else:
            return None

    def upcoming(self):
        today = date.today()
        return self.filter(start__gte=today)

    def previous(self):
        today = date.today()
        return self.filter(end__lte=today)


class Term(models.Model):
    termid = models.SlugField(max_length=6, primary_key=True)
    name = models.CharField(max_length=20)
    start = models.DateField(null=True)
    end = models.DateField(null=True)

    objects = TermManager()

    def save(self):
        if self.name is None:
            self.name = self.termid
        super(Term, self).save()


    def breadcrumbs(self):
        return [(self.name, self.get_absolute_url())]

    def get_absolute_url(self):
        return "/%s/" % self.termid

    def __unicode__(self):
        return self.termid


class Course(models.Model):
    name = models.CharField(max_length=60)
    courseid = models.SlugField(max_length=10)
    description = models.TextField(default="",blank=True)
    term = models.ForeignKey(Term, related_name="courses")

    def breadcrumbs(self):
        return self.term.breadcrumbs() + [(self.courseid, self.get_absolute_url())]

    def get_absolute_url(self):
        return "/%s/%s/" % (self.term.termid, self.courseid)

    def __unicode__(self):
        return self.courseid

    def save(self):
        if self.name is None:
            self.name = self.courseid
        super(Course, self).save()

    class Meta:
        ordering = ["term", "courseid"]
