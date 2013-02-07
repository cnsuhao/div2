from django.db import models
from django.contrib.contenttypes.models import ContentType
from django.contrib.contenttypes import generic

class WorkingCopy(models.Model):
    content_type = models.ForeignKey(ContentType)
    object_id = models.PositiveIntegerField()
    content_object = generic.GenericForeignKey()

    data = models.TextField()
  
    last_modified = models.DateTimeField(null=True)
    copy_created  = models.DateTimeField(null=True)
    
    class Meta():
        order_with_respect_to = 'content_type'
        ordering = ['content_type']

