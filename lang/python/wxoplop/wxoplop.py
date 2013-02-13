import os

# try to import some systems clipboard
try:
    import win32clipboard
except ImportError:
    pass 
import wx

import settings
import clipboard
from fields import *
from helpers import *

#  Dialog

#  Status
STAT_ID = wx.NewId()
STAT_INITIAL = "Oplop Password"

#  Field
NICK_DEFAULT = "Nickname..."

PASS_DEFAULT = "Master Password..."
PASS_HIDDEN =  "tmp"


FIELD_FLAGS = wx.ALIGN_CENTER|wx.EXPAND

#  Information

#  Buttons    
CLBD_ID = wx.NewId()
CLBD_TEXT = "Copy to Clipboard"

DONE_ID = wx.NewId()
DONE_TEXT = "Finished"

class OplopFrame(wx.Frame):
    """Simple oplop password frame"""
    title = "Oplop"
    style =  wx.SYSTEM_MENU | wx.CLOSE_BOX | wx.CAPTION #| wx.FRAME_NO_TASKBAR | wx.STAY_ON_TOP
    
    def __init__(self):
        self.id = wx.NewId()
        wx.Frame.__init__(self, None, self.id, title=settings.DLG_CAPTION, style=self.style)
        
        # status
        self.status = wx.StaticText(self, STAT_ID)
        
        # nickname
        self.nickname = TextField(self, missing="Nickname...", tooltip="Type a nickname (eg Amazon, AMZN, or bookstore)")
        self.password = TextField(self, missing="Master Password...", tooltip="Type your master password")
        self.confirm = TextField(self, missing="Confirm Password...", tooltip="Validate your master password")
        
        cfrmSizer = wx.BoxSizer(wx.HORIZONTAL)
        cfrmSizer.Add(self.confirm, 0, FIELD_FLAGS)
                
        # buttons
        bttnSizer = wx.BoxSizer(wx.HORIZONTAL)
        bttnSizer.Add(wx.Button(self, CLBD_ID, CLBD_TEXT))
        bttnSizer.Add(wx.Button(self, DONE_ID, DONE_TEXT))
        
        # add the sizers
        dialogSizer = wx.BoxSizer(wx.VERTICAL)
        dialogSizer.Add(self.status, 0, wx.ALIGN_CENTER)
        dialogSizer.Add(self.nickname, 0, FIELD_FLAGS)
        dialogSizer.Add(self.password, 0, FIELD_FLAGS)
        dialogSizer.Add(cfrmSizer, 0, wx.ALIGN_CENTER)
        dialogSizer.Add(bttnSizer, 0, wx.ALIGN_RIGHT)
        
        # events
        self.Bind(wx.EVT_BUTTON, self.CopyToClipboard, id=CLBD_ID)
        self.Bind(wx.EVT_BUTTON, self.OnFinished, id=DONE_ID)
        
        #self.Bind(wx.EVT_COMMAND_TAB, 
        
        # get ready to display
        self.SetSizer(dialogSizer)
        self.Center()
        self.Redraw()
        self.status.SetLabel(STAT_INITIAL)
        self.status.SetFocus()
        self.Show()
    
    def Redraw(self):
        return
    
    def GetValue(self):
        return "password goes here"
    
    # Events
    def Nothing(self, event):
        pass
    
    def Debug(self, event):
        print event.GetId(), event.GetEventType()
    
    def CopyToClipboard(self, event):
        pass
        # windows clipboard, skipping for now
        # win32clipboard.OpenClipboard()
        # win32clipboard.EmptyClipboard()
        # win32clipboard.SetClipboardData(win32clipboard.CF_TEXT, self.GetValue())
        # win32clipboard.CloseClipboard()
    
    def OnFinished(self, event):
        self.Destroy()

        
if __name__=='__main__':
    import sys
     

    app = wx.App(False)
    OplopFrame()
    app.MainLoop()
