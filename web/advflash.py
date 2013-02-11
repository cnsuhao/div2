"""
AdvFlash: A Pylons helper class

In pylons, it is easy to "flash" messages to form data.  So, as you go along
processing an input form, it is customary to flash errors that will need to be 
validated later.

Instead, I wanted to create a mini DSL to let me set the error level in the call.

So, now flashing can work like this.

    flash = AdvFlash()
    ...
    flash("Hello")
    flash.error("HELP, EVERYTHING IS WRONG")
    flash.info("User record updated")

Essentially, the code passes the name of any attribute it doesn't know about as the 
type for the flash.


Usually, the code rendering this would simply do:
    flash("message")   => <div class="flash info">message</div>
    flash.error("go away")   => <div class="flash error">go away</div>

Though that is entirely up to the programmer once the messages are popped.
"""


from functools import partial

class AdvFlash(object):    
    class Message(object):
        def __init__(self,v): self.text = v
        def __repr__(self): return self.text
        def __str__(self): return self.text
        def __unicode(self): return unicode(self.text)
    
    def __init__(self, session_key='flash', typ="info", **kw):
        self.key = session_key
        self.type = typ
        self.defaults = kw
        
    def __call__(self, message, typ=None, **kw):
        from pylons import session
        message = AdvFlash.Message(message)
        message.type = typ or self.type

        info = self.defaults.copy()
        info.update(kw)

        for k,v in info.items():
            setattr(message,k,v)

        session.setdefault(self.key, []).append(message)
        session.save()
    
    def __getattr__(self,k):
        "for any given attribute, simply return a partial with the name as the type"
        return partial(self, typ=k)

    def pop_messages(self):
        from pylons import session
        messages = session.pop( self.key, [] )
        session.save()
        return messages
