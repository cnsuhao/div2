import wx

class TextField(wx.TextCtrl):
    visited = False
    visible = True
    value = ""
    style = wx.CENTER #| wx.TE_PROCESS_ENTER | wx.TE_PROCESS_TAB
    
    cMissing = None
    
    def __init__(self, parent, missing=None, tooltip=None):
        self.id = wx.NewId()
        wx.TextCtrl.__init__(self, parent, self.id, style=self.style)
        
        self.sMissing = missing if missing is not None else ""
        
        if tooltip is not None:
            self.SetToolTipString(tooltip)
        
        self.cDefault = self.GetForegroundColour()
        
        if not hasattr(parent, 'fields'):
            parent.fields = []
        parent.fields.append(self)
    
    def Hide(self):
        self.visible = False
        wx.TextCtrl.Show(self,False)
    
    def Show(self, state):
        self.visible = state
        wx.TextCtrl.Show(self,state)
    
    # Color setting methods
    def SetTextColor(self, color): 
        self.SetForegroudColor(color)
        
    def SetForegroundColour(self, color):
        self.cDefault = color
        
    def SetMissingColor(self, color):
        self.cMissing = color
    
        
class PasswordField(TextField):
    def __init__(self, parent):
        OplopField.__init__(self, parent)