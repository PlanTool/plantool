#coding:utf-8
import re
import wx
import time
import commands


class PlannerUI(wx.Frame):
    def __init__(self):
        self.homedir = "/home/fengwf/Documents/Planner/"
        self.pddldir = self.homedir + 'Domains/IPC1_1998/gripper/'
        self.ff = "Neoclassical-Planning/Graphplan/FF/FF-v2.3/ff"
        self.dom_file = 'domain.pddl'
        self.pro_file = 'problem.pddl'
        
        wx.Frame.__init__(self, None, -1, 'PlanTool', size=(800, 600))  #size of UI
        panel = wx.Panel(self, -1)
        #panel.SetBackgroundColour('aquamarine')
        panel.Refresh()
        
        #App's name
        self.app_name = wx.StaticText(panel, -1, "PlanTool", (50, 50))
        self.app_name_font = wx.Font(36, wx.ROMAN, wx.ITALIC, wx.BOLD)
        self.app_name.SetFont(self.app_name_font)

        #Listbox
        wx.StaticText(panel, -1, "Select a planner:", (20,150))
        self.PlannerList = ['Blackbox', 'IPP', 'FF', 'LPG', 'MIPS-BDD', 'AltAlt', 'SHOP', 'CPT',
        'SATplan06', 'Lama08',  'Lama11', 'SGplan522', 'Randward']
        #self.listBox = wx.ListBox(panel, -1, (600, 50), (80, 120), self.PlannerList, wx.LB_SINGLE)
        #self.listBox.SetSelection(0)
        self.listBox = wx.Choice(panel, -1, (200, 150), choices=self.PlannerList)

        #Chose domain file and problem file
        wx.StaticText(panel, -1, "Chose a domain:", (20,250), style=wx.ALIGN_LEFT)
        wx.StaticText(panel, -1, "Chose a problem:", (20,300), style=wx.ALIGN_LEFT)
        self.dom_button = wx.Button(panel, -1,"Chose", pos=(150, 250), size=(180, -1))  
        self.Bind(wx.EVT_BUTTON, self.Domain_Button, self.dom_button)
        self.dom_button.SetDefault()
        self.pro_button = wx.Button(panel, -1,"Chose", pos=(150, 300), size=(180, -1))  
        self.Bind(wx.EVT_BUTTON, self.Problem_Button, self.pro_button)
        self.pro_button.SetDefault()

        #Input textbox
        '''wx.StaticText(panel, -1, "path for operator \nand fact file", (20,250), style=wx.ALIGN_LEFT)
        self.filepath = wx.TextCtrl(panel, -1, "", size=(175, -1), pos=(150,250), style=wx.TE_PROCESS_ENTER)
        self.filepath.SetInsertionPoint(0)

        wx.StaticText(panel, -1, "operator file name", (20,300), style=wx.ALIGN_LEFT)
        self.operator = wx.TextCtrl(panel, -1, "", size=(175, -1), pos=(150,300), style=wx.TE_PROCESS_ENTER)
        self.operator.SetInsertionPoint(0)

        wx.StaticText(panel, -1, "fact file name", (20,350), style=wx.ALIGN_LEFT)
        self.problem = wx.TextCtrl(panel, -1, "", size=(175, -1), pos=(150,350), style=wx.TE_PROCESS_ENTER)
        self.problem.SetInsertionPoint(0)
        #self.Bind(wx.EVT_TEXT_ENTER,self.OnText,self.InputText)'''


        #Output textbox
        output_label = wx.StaticText(panel, -1, "Realtime output", (340,20), (440,60), wx.ALIGN_CENTER)
        output_label_font = wx.Font(18, wx.NORMAL, wx.NORMAL, wx.NORMAL)
        output_label.SetFont(output_label_font)
        self.OutputText = wx.TextCtrl(panel, -1,"",
        size=(440, 420), pos=(340,60), style=wx.TE_MULTILINE | wx.TE_READONLY | wx.TE_RICH2) 
        output_font = wx.Font(12, wx.DEFAULT, wx.NORMAL, wx.NORMAL)
        self.OutputText.SetFont(output_font)

        #
        self.button1 = wx.Button(panel, -1,"Test", pos=(100, 500))  
        self.Bind(wx.EVT_BUTTON, self.Test_Button, self.button1)
        self.button1.SetDefault()

        #
        self.button2 = wx.Button(panel, -1,"Run", pos=(200, 500))  
        self.Bind(wx.EVT_BUTTON, self.Run_Button, self.button2)
        self.button2.SetDefault()

        #
        self.button6 = wx.Button(panel, -1,"Exit", pos=(600, 500))  
        self.Bind(wx.EVT_BUTTON, self.Exit_Button, self.button6)
        self.button6.SetDefault()


    def Domain_Button(self, event):
        dom_type = "PDDL source (*.pddl)|*.pddl|" \
        "Text source (*.txt)|*.txt|" \
        "All files (*.*)|*.*"
        dialog = wx.FileDialog(None, "Choose a file", self.pddldir, "", 
            dom_type, wx.OPEN)
        if dialog.ShowModal() == wx.ID_OK:
            self.dom_file = dialog.GetPath() 
            self.dom_button.SetLabelText(re.sub(r'.*/','',self.dom_file))
            print "\nDomain file chosen:%s\n"%self.dom_file
        dialog.Destroy()


    def Problem_Button(self, event):
        pro_type = "PDDL source (*.pddl)|*.pddl|" \
        "Text source (*.txt)|*.txt|" \
        "All files (*.*)|*.*"
        dialog = wx.FileDialog(None, "Choose a file", self.pddldir, "", 
            pro_type, wx.OPEN)
        if dialog.ShowModal() == wx.ID_OK:
            self.pro_file = dialog.GetPath()
            self.pro_button.SetLabelText(re.sub(r'.*/','',self.pro_file)) 
            print "\nProblem file chosen:%s\n"%self.pro_file
        dialog.Destroy()


    def Test_Button(self, event):
        self.pointer = self.OutputText.GetInsertionPoint()
        main = self.homedir + self.ff + ' -o ' + self.dom_file + ' -f ' + self.pro_file
        self.OutputText.AppendText("\nmain = %s\n"%main)
        tp_point = self.pointer
        self.pointer = self.OutputText.GetInsertionPoint()
        self.OutputText.SetStyle(tp_point, self.pointer, wx.TextAttr("blue", wx.NullColour))  #设定颜色
        result = commands.getoutput(main)
        self.OutputText.AppendText("\n\nResult: %s\n"%result)


    def Run_Button(self, event):
        ind = self.listBox.GetSelection()
        tool_name = self.PlannerList[ind]
        self.OutputText.AppendText("Planner Selected: %s\n"%tool_name)
        print tool_name


    def Exit_Button(self, event):
        wx.Exit()


    def OnText(self, event):
        pass


        
if __name__ == '__main__':
    start_time = time.time()
    app = wx.PySimpleApp()
    frame = PlannerUI()
    frame.Show()
    app.MainLoop()