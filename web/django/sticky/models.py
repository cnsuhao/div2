from django.db import models

# Create your models here.

class Note(models.Model):
    text  = models.TextField(default="double click to edit")
    x     = models.PositiveIntegerField(default=10)
    y     = models.PositiveIntegerField(default=10)
    w     = models.PositiveIntegerField(default=300)
    h     = models.PositiveIntegerField(default=200)
    ind   = models.PositiveIntegerField(default=0)
    trash = models.BooleanField()

    def __unicode__(self):
        return self.text

    def bring_to_top(self):
        notes = Note.objects.filter(trash=False)
        for n in notes:
            n.ind -= 1
            n.save()
        self.ind = len(notes)

    class Meta:
        ordering = ['ind']
