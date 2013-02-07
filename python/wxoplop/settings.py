import os
import wxoplop

APP_NAME = "wxOplop"
VERSION = '0.2'

# get the config path
if os.getenv("HOME") is not None:
    CFG_PATH = os.path.join(os.getenv("HOME"), "." + APP_NAME.lower())
elif os.getenv("APPDATA") is not None:
    CFG_PATH = os.path.join(os.getenv("APPDATA"), APP_NAME.lower() + ".cfg")
    
def createDefault():
    from helpers import filecopy
    default = os.path.join(os.path.dirname(__file__), APP_NAME.lower() + ".cfg")
    filecopy(default, CFG_PATH)
    
if not os.path.isfile(CFG_PATH) or True:    # temporarily true for debuging purposes
    createDefault()
    
    
# Dialog settings
DLG_CAPTION = "%s v%s" % (APP_NAME, VERSION)
