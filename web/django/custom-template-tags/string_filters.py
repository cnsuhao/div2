# truncatechars

from django.template import Library
from django.template.defaultfilters import stringfilter

register = Library()

@register.filter(name="truncatechars")
def truncatechars(value, arg):
	if len(value)<=arg:
		return value
	else:
		return "%s..." % value[:arg]
truncatechars.is_safe=False
