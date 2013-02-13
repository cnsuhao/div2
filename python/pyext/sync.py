"""
A simple mutex Lock decorator.  
"""

from functools import wraps
from threading import Lock

def locked(lock, blocking=True):

    # not a valid lock object if no acquire method
    if not hasattr(lock, "acquire"):
        raise AttributeError("locked takes an object with an 'acquire([blocking])' method")

    def decorator(f):
        @wraps(f)
        def wrapper(*a, **kw):
            result = None
            if lock.acquire(blocking):
                result = f(*a, **kw)
            lock.release()
            return result
        return wrapper
    return decorator


if __name__ == "__main__":
    lck = Lock()

    @locked(lck)
    def f():
        print "I need the lock, and it is locked: %r" % lck.locked()

    f()
