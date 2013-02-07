# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file 'lib/pytimesheet/timesheet.ui'
#
# Created: Mon Jul 21 04:13:23 2008
#      by: PyQt4 UI code generator 4.3.3
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_Window(object):
    def setupUi(self, Window):
        Window.setObjectName("Window")
        Window.resize(QtCore.QSize(QtCore.QRect(0,0,475,358).size()).expandedTo(Window.minimumSizeHint()))

        font = QtGui.QFont()
        font.setWeight(50)
        font.setItalic(False)
        font.setUnderline(False)
        font.setBold(False)
        Window.setFont(font)
        Window.setAcceptDrops(False)
        Window.setSizeGripEnabled(False)

        self.label = QtGui.QLabel(Window)
        self.label.setGeometry(QtCore.QRect(250,30,81,31))

        font = QtGui.QFont()
        font.setPointSize(14)
        font.setWeight(75)
        font.setBold(True)
        self.label.setFont(font)
        self.label.setAlignment(QtCore.Qt.AlignLeading|QtCore.Qt.AlignLeft|QtCore.Qt.AlignVCenter)
        self.label.setMargin(4)
        self.label.setObjectName("label")

        self.appname = QtGui.QLabel(Window)
        self.appname.setGeometry(QtCore.QRect(10,10,471,34))

        font = QtGui.QFont()
        font.setPointSize(19)
        font.setWeight(75)
        font.setItalic(True)
        font.setUnderline(True)
        font.setBold(True)
        self.appname.setFont(font)
        self.appname.setObjectName("appname")

        self.status = QtGui.QLabel(Window)
        self.status.setGeometry(QtCore.QRect(330,30,131,31))

        palette = QtGui.QPalette()

        brush = QtGui.QBrush(QtGui.QColor(255,0,0))
        brush.setStyle(QtCore.Qt.SolidPattern)
        palette.setBrush(QtGui.QPalette.Active,QtGui.QPalette.WindowText,brush)

        brush = QtGui.QBrush(QtGui.QColor(255,0,0))
        brush.setStyle(QtCore.Qt.SolidPattern)
        palette.setBrush(QtGui.QPalette.Active,QtGui.QPalette.Text,brush)

        brush = QtGui.QBrush(QtGui.QColor(255,0,0))
        brush.setStyle(QtCore.Qt.SolidPattern)
        palette.setBrush(QtGui.QPalette.Inactive,QtGui.QPalette.WindowText,brush)

        brush = QtGui.QBrush(QtGui.QColor(255,0,0))
        brush.setStyle(QtCore.Qt.SolidPattern)
        palette.setBrush(QtGui.QPalette.Inactive,QtGui.QPalette.Text,brush)

        brush = QtGui.QBrush(QtGui.QColor(255,0,0))
        brush.setStyle(QtCore.Qt.SolidPattern)
        palette.setBrush(QtGui.QPalette.Disabled,QtGui.QPalette.WindowText,brush)

        brush = QtGui.QBrush(QtGui.QColor(255,0,0))
        brush.setStyle(QtCore.Qt.SolidPattern)
        palette.setBrush(QtGui.QPalette.Disabled,QtGui.QPalette.Text,brush)
        self.status.setPalette(palette)

        font = QtGui.QFont()
        font.setPointSize(14)
        font.setWeight(75)
        font.setItalic(True)
        font.setBold(True)
        self.status.setFont(font)
        self.status.setAutoFillBackground(False)
        self.status.setStyleSheet("color: red;")
        self.status.setFrameShape(QtGui.QFrame.StyledPanel)
        self.status.setLineWidth(1)
        self.status.setAlignment(QtCore.Qt.AlignLeading|QtCore.Qt.AlignLeft|QtCore.Qt.AlignVCenter)
        self.status.setMargin(4)
        self.status.setObjectName("status")

        self.gridLayoutWidget = QtGui.QWidget(Window)
        self.gridLayoutWidget.setGeometry(QtCore.QRect(0,60,471,301))
        self.gridLayoutWidget.setObjectName("gridLayoutWidget")

        self.gridlayout = QtGui.QGridLayout(self.gridLayoutWidget)
        self.gridlayout.setMargin(10)
        self.gridlayout.setObjectName("gridlayout")

        self.hboxlayout = QtGui.QHBoxLayout()
        self.hboxlayout.setObjectName("hboxlayout")

        self.tag_field = QtGui.QLineEdit(self.gridLayoutWidget)
        self.tag_field.setEnabled(True)

        font = QtGui.QFont()
        font.setFamily("Monospace")
        self.tag_field.setFont(font)
        self.tag_field.setObjectName("tag_field")
        self.hboxlayout.addWidget(self.tag_field)
        self.gridlayout.addLayout(self.hboxlayout,1,1,1,1)

        self.project_field_label = QtGui.QLabel(self.gridLayoutWidget)

        font = QtGui.QFont()
        font.setPointSize(10)
        self.project_field_label.setFont(font)
        self.project_field_label.setAlignment(QtCore.Qt.AlignRight|QtCore.Qt.AlignTrailing|QtCore.Qt.AlignVCenter)
        self.project_field_label.setObjectName("project_field_label")
        self.gridlayout.addWidget(self.project_field_label,0,0,1,1)

        self.project_field = QtGui.QLineEdit(self.gridLayoutWidget)
        self.project_field.setObjectName("project_field")
        self.gridlayout.addWidget(self.project_field,0,1,1,1)

        self.task_field_label = QtGui.QLabel(self.gridLayoutWidget)

        font = QtGui.QFont()
        font.setPointSize(10)
        font.setWeight(50)
        font.setBold(False)
        self.task_field_label.setFont(font)
        self.task_field_label.setAlignment(QtCore.Qt.AlignRight|QtCore.Qt.AlignTrailing|QtCore.Qt.AlignVCenter)
        self.task_field_label.setObjectName("task_field_label")
        self.gridlayout.addWidget(self.task_field_label,2,0,1,1)

        self.task_field = QtGui.QTextEdit(self.gridLayoutWidget)
        self.task_field.setEnabled(True)

        palette = QtGui.QPalette()

        brush = QtGui.QBrush(QtGui.QColor(255,255,255))
        brush.setStyle(QtCore.Qt.SolidPattern)
        palette.setBrush(QtGui.QPalette.Active,QtGui.QPalette.Base,brush)

        brush = QtGui.QBrush(QtGui.QColor(255,255,255))
        brush.setStyle(QtCore.Qt.SolidPattern)
        palette.setBrush(QtGui.QPalette.Inactive,QtGui.QPalette.Base,brush)

        brush = QtGui.QBrush(QtGui.QColor(239,235,231))
        brush.setStyle(QtCore.Qt.SolidPattern)
        palette.setBrush(QtGui.QPalette.Disabled,QtGui.QPalette.Base,brush)
        self.task_field.setPalette(palette)
        self.task_field.setContextMenuPolicy(QtCore.Qt.CustomContextMenu)
        self.task_field.setObjectName("task_field")
        self.gridlayout.addWidget(self.task_field,2,1,1,1)

        self.hboxlayout1 = QtGui.QHBoxLayout()
        self.hboxlayout1.setObjectName("hboxlayout1")

        self.sessionButton = QtGui.QPushButton(self.gridLayoutWidget)

        font = QtGui.QFont()
        font.setPointSize(11)
        font.setWeight(75)
        font.setBold(True)
        self.sessionButton.setFont(font)
        self.sessionButton.setObjectName("sessionButton")
        self.hboxlayout1.addWidget(self.sessionButton)

        self.buttonBox = QtGui.QDialogButtonBox(self.gridLayoutWidget)
        self.buttonBox.setOrientation(QtCore.Qt.Horizontal)
        self.buttonBox.setStandardButtons(QtGui.QDialogButtonBox.Cancel|QtGui.QDialogButtonBox.Ok)
        self.buttonBox.setObjectName("buttonBox")
        self.hboxlayout1.addWidget(self.buttonBox)
        self.gridlayout.addLayout(self.hboxlayout1,3,1,1,1)

        self.tag_field_label = QtGui.QLabel(self.gridLayoutWidget)

        font = QtGui.QFont()
        font.setPointSize(10)
        self.tag_field_label.setFont(font)
        self.tag_field_label.setAlignment(QtCore.Qt.AlignRight|QtCore.Qt.AlignTrailing|QtCore.Qt.AlignVCenter)
        self.tag_field_label.setObjectName("tag_field_label")
        self.gridlayout.addWidget(self.tag_field_label,1,0,1,1)

        self.line = QtGui.QFrame(Window)
        self.line.setGeometry(QtCore.QRect(0,50,471,16))
        self.line.setFrameShape(QtGui.QFrame.HLine)
        self.line.setFrameShadow(QtGui.QFrame.Sunken)
        self.line.setObjectName("line")

        self.retranslateUi(Window)
        QtCore.QObject.connect(self.sessionButton,QtCore.SIGNAL("clicked()"),Window.changeSessionStatus)
        QtCore.QObject.connect(self.buttonBox,QtCore.SIGNAL("accepted()"),Window.accept)
        QtCore.QObject.connect(self.buttonBox,QtCore.SIGNAL("rejected()"),Window.reject)
        QtCore.QObject.connect(self.project_field,QtCore.SIGNAL("returnPressed()"),Window.checkin)
        QtCore.QMetaObject.connectSlotsByName(Window)

    def retranslateUi(self, Window):
        Window.setWindowTitle(QtGui.QApplication.translate("Window", "PyTimesheet GUI", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setText(QtGui.QApplication.translate("Window", "Status:", None, QtGui.QApplication.UnicodeUTF8))
        self.appname.setText(QtGui.QApplication.translate("Window", "PyTimesheet GUI", None, QtGui.QApplication.UnicodeUTF8))
        self.status.setText(QtGui.QApplication.translate("Window", "Checked Out", None, QtGui.QApplication.UnicodeUTF8))
        self.tag_field.setToolTip(QtGui.QApplication.translate("Window", "Tags separated by space", None, QtGui.QApplication.UnicodeUTF8))
        self.tag_field.setStatusTip(QtGui.QApplication.translate("Window", "Tags separated by space", None, QtGui.QApplication.UnicodeUTF8))
        self.project_field_label.setText(QtGui.QApplication.translate("Window", "Project:", None, QtGui.QApplication.UnicodeUTF8))
        self.task_field_label.setText(QtGui.QApplication.translate("Window", "Task:", None, QtGui.QApplication.UnicodeUTF8))
        self.sessionButton.setText(QtGui.QApplication.translate("Window", "Checkin", None, QtGui.QApplication.UnicodeUTF8))
        self.tag_field_label.setText(QtGui.QApplication.translate("Window", "Tags:", None, QtGui.QApplication.UnicodeUTF8))

