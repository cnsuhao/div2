import ctypes

def isOpened(f):
    def wrapper(self,*a,**kw):
        if self._opened:
            return f(self,*a,**kw)
    return wrapper

class DummyClipboard(object):
    def __enter__(self):
        return self
        
    def __exit__(self, type, value, traceback):
        return 
        
    def open(self):
        return
        
    def close(self):
        return
        
    def get(self):
        return
        
    def set(self, value):
        return

    
class Win32Clipboard(object):
    CF_TEXT = 1
    GMEM_MOVEABLE = 0x0002
    GMEM_ZEROINT = 0x0040

    def __init__(self):
        self.handle = 0
        self._opened = False
    
    def __enter__(self):
        self.open()
        return self
        
    def __exit__(self, type, value, traceback):
        self.close()
        return isinstance(value, TypeError)
    
    def open(self):
        ctypes.windll.user32.OpenClipboard(self.handle)
        self._opened = True
        
    def close(self):
        ctypes.windll.user32.CloseClipboard()
        self._opened = False
    
    @isOpened
    def get(self):
        ptr = ctypes.windll.user32.GetClipboardData(self.CF_TEXT)
        val = ctypes.c_char_p(ptr).value
        return val
    
    @isOpened
    def set(self, value):
        data = str(value)
        
        mem = ctypes.windll.kernel32.GlobalAlloc(self.GMEM_MOVEABLE | self.GMEM_ZEROINT, len(data) + 1)  # +1 is for null byte
        ptr = ctypes.windll.kernel32.GlobalLock(mem)
        ctypes.cdll.msvcrt.strncpy(ptr, data, len(data))
        ctypes.windll.kernel32.GlobalUnlock(mem)
        
        ctypes.windll.user32.EmptyClipboard()
        ctypes.windll.user32.SetClipboardData(self.CF_TEXT, mem)

# initialze clipboard        
clipboard = DummyClipboard()

if hasattr(ctypes,"windll"):
    clipboard = Win32Clipboard()
    

