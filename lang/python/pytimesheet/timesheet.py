"""
    A library to provide a simple timesheet system.  This may be expanded as the need arises.  It uses a simple log system to check various things in.  This can potentially be integrated into a larger scale project but for now is intended to be simply bound to keyboard commands.  Hopefully will create a more robust timesheet system.
    
    Features:
        checkin: equivilent of "punching in" for the work period
        new task: get a timestamp and a description of what is being worked on, mark start time
        end task: stop previous task
        switch task: auto start/stop
        checkout: mark work period as over and move fild out of active log
        
    To Add:
        various ways to fetch the data, as is will be written out in xml for xsl purposes
        GUI
        link in with g15 daemon
"""

from xml.etree import ElementTree as ET
from datetime import datetime
import dateutil as util

import os.path

class TimeSheet():
    def __init__(self, project):
        # set the project name
        self.project = project

        # get project path
        homedir = os.path.expanduser('~')
        root = os.path.join(homedir,'.timesheets')
        if not os.path.isdir(root):
            os.mkdir(root)
        self.path = os.path.join(root,project+'.xml')
        
        # get the file data
        if not os.path.isfile(self.path):
            self.makeTimesheet()
        else:
            self.tree = ET.parse(self.path)
        
    def checkin(self):
        """
            This function takes a project name and opens the "active.log" in the ~/.timesheets/project/.  If either folder DNE, it creates it.
        
            It then checks if there is a previous active.log.  If there is, it points out the previous day was never checked out and then see if it was 
            at least stopped.  If not, it saves the previous active.log flagged with errors.
        """
        
        session = ET.SubElement(self.tree.getroot(), "session")
        self.save()

    def switch(self, description,tags):
        self.stopTask()
        self.startTask(description,tags)
        self.save()
        
    def start(self, description,tags):
        self.startTask(description,tags)
        self.save()
        
    def stop(self):
        self.stopTask()
        self.save()

    def save(self):
        self.tree.write(self.path)

    def startTask(self, description, tags):
        last_session = self.tree.findall("session")[-1]
        task = ET.SubElement(last_session, "task")
        task.set("start", datetime.now().isoformat())
        task.text = description
        for tag in tags:
            new_tag = ET.SubElement(task, "tag")
            new_tag.text = tag
        print self.tree

    def stopTask(self):
        last_session = self.tree.findall("session")[-1]
        tasks = last_session.findall("task")
        if len(tasks)==0:
            return
        last_task    = tasks[-1]
        last_task.set("stop", datetime.now().isoformat())

    def makeTimesheet(self):
        root = ET.Element("timesheet")
        root.set("project",self.project)
        self.tree = ET.ElementTree(root)
        self.save()
