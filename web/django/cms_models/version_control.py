from django.db import models
from copy import copy as copy_object

class CannotRevert(Exception): pass

# Create your models here.
class _ActiveManager(models.Manager):
    def get_query_set(self):
        return super(_ActiveManager, self).get_query_set().filter(_TrackHistory_current=True)

class TrackHistory(models.Model):
    _TrackHistory_current  = models.BooleanField(default=True, editable=False)
    _TrackHistory_revision = models.PositiveIntegerField(default=0, editable=False)
    
    objects  = _ActiveManager()
    _objects = models.Manager()
    
    def _save(self):
        super(self.__class__,self).save()

    def save(self):
        
        
        if self.id and not getattr(self,'_TrackHistory__copied', False):
            old = copy_object(self)
            old._TrackHistory_current=False
            old._save()
            
            self.id = None
            self._TrackHistory_current=True
            self._TrackHistory_revision += 1
            
        super(TrackHistory,self).save()
        
        print self.__class__.objects.all()
        
    def _save(self):
        super(TrackHistory,self).save()

    @classmethod
    def _clear_history_after(cls, rev):
        objs = cls._objects.filter(_TrackHistory_revision=rev)
        for obj in objs:
            objs.delete()
    
    @classmethod
    def _revert(cls,rev,**kwargs):
        try:
            kwargs['_revision'] = rev
            for k in kwargs:
                print [k]
            obj = cls._objects.get(**kwargs)
            old = cls.objects.get(**kwargs)
            old._Track_History_current = False
            obj._Track_History_current = True
            old._save()
            obj._save()
        except cls.DoesNotExist: raise CannotRevert('obj/revision number does not exist')
        
    def has_changed(self,obj):
        for k in self.__dict__:
            if self.__dict__[k] != obj.__dict__[k]:
                return True
        return False

    class Meta():
        abstract = True