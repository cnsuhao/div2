from django.db.models import get_model
from django.template import Library, Node, TemplateSyntaxError

register = Library()

class LoadAll(Node):
	def __init__(self,model,var):
		self.var = var
		self.model = get_model(*model.split('.'))
		
	def render(self, context):
		context[self.var] = self.model._default_manager.all()
		return ''
		
def get_all(parser, token):
	bits = token.contents.split()
	if len(bits) != 4:
		raise TemplateSyntaxError, "get_latest tag takes exactly three arguments"
	if bits[2] != 'as':
		raise TemplateSyntaxError, "second argument to get_latest tag must be 'as'"
	return LoadAll(bits[1], bits[3])
	
get_all = register.tag(get_all)

