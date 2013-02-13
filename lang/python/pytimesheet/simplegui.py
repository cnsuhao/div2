#!/usr/bin/python

import sys, os

from timesheet import TimeSheet
from template import Ui_Window

import PyQt4
from PyQt4 import QtCore, QtGui, uic
from PyQt4.QtCore import QString, QPoint
from PyQt4.QtGui import *


class TimesheetGui(QDialog,Ui_Window):
    def __init__(self, *args):
            
                # if checkin status doesn't change, do a switch
        self.switch = True
        
        # check for the last set project
        home = os.path.expanduser('~')
        self.timesheetdir = timesheetdir = os.path.join(home, '.timesheets')
        if not os.path.isdir(timesheetdir):
            os.mkdir(timesheetdir)
        self.session = os.path.join(timesheetdir, '.session')
        if not os.path.isfile(self.session):
            self.project=None
        else:
            f = open(self.session)
            self.project = f.read()
            f.close()
    
        self.loadProject()

        # open tags file
        tags = os.path.join(timesheetdir, '.tags')
        if not os.path.isfile(tags):
            self.all_tags=[]
        else:
            f = open(tags)
            self.all_tags = f.readlines()
            f.close()
    
        # initalize UI
        QWidget.__init__(self, *args)
        self.setupUi(self)
        self.updateUI()
        
        # add tag completer
        tags = QtCore.QStringList(self.all_tags)
        self.completer = QCompleter(tags,self)
        self.tag_field.setCompleter(self.completer)
    
    def loadProject(self):
        # load project if there is one
        if self.project:
            self.timesheet = TimeSheet(self.project)
    
    def updateUI(self):

        if self.project:
            self.project_field.setText(self.project)
            self.project_field.setEnabled(False)
            self.tag_field.setEnabled(True)
            self.task_field.setEnabled(True)
            self.task_field.setFocus()
            self.sessionButton.setText('Checkout')
            self.status.setText('Checked In')
            self.status.setStyleSheet('color: green')
        else:
            self.project_field.clear()
            self.project_field.setEnabled(True)
            self.tag_field.setEnabled(False)
            self.tag_field.clear()
            self.task_field.setEnabled(False)
            self.task_field.clear()
            self.project_field.setFocus()
            self.sessionButton.setText('Checkin')
            self.status.setText('Checked Out')
            self.status.setStyleSheet('color: red')
    
    def rejected(self):
        sys.exit()    
    
    def saveTags(self):
        f = open(os.path.join(self.timesheetdir,'.tags'),'a')
        tag = self.tag_field.text().__str__()
        try:
            if self.all_tags.index(tag) < 0:
                pass
        except ValueError:
            f.write('\n'+tag)
        f.close()
        
    def changeSessionStatus(self):
        self.switch = False
        if self.project:
            self.checkout()
        else:
            self.checkin()
        
    def checkout(self):
        self.timesheet.stop()
        self.timesheet = None
        self.project = None
        os.remove(self.session)
        self.updateUI()
        
    def checkin(self):
        self.project = self.project_field.text().__str__()
        if self.project == '':
            return
        
        f = open(self.session,'w')
        f.write(self.project)
        f.close()
        self.loadProject()
        self.timesheet.checkin()
        self.updateUI()
    
    def accept(self):
        if not self.project:
            self.reject()
        if self.switch:
            self.timesheet.stop()
        self.timesheet.start(self.task_field.toPlainText().__str__(),self.tag_field.text().__str__().split(' '))
        self.saveTags()
        self.done(0)

    def keyPressEvent(self, event):
        pass


app = QApplication(sys.argv)
widget = TimesheetGui()
widget.show()
app.exec_()
