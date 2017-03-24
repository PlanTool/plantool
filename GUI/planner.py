#coding:utf-8
import re
import wx
import os
import sys
import time
import wx.html
import wx.lib.buttons as buttons
from ctypes import *


class PlannerGUI(wx.Frame):
    def __init__(self):
        print 'Initializing PlannerGUI...'
        self.homedir = os.getcwd()
        self.pddldir = './pddl_files'#self.homedir + 'Domains/IPC1_1998/gripper/'
        self.ff = "Neoclassical-Planning/Graphplan/FF/FF-v2.3/ff"
        self.dom_file = ''
        self.pro_file = ''
        self.frame_style = wx.DEFAULT_FRAME_STYLE

        wx.Frame.__init__(self, None, -1, 'PlanTool', style=self.frame_style,
        size=(1000, 750))  
        self.panel = wx.Panel(self, -1)
        self.panel.Refresh()
        self.count = 0
        self.out_size = 11
        self.out_style = wx.DEFAULT
        
        #App's name
        self.app_name = wx.StaticText(self.panel, -1, "Deterministic\n   Planning", (15, 15))
        self.app_name_font = wx.Font(32, wx.ROMAN, wx.ITALIC, wx.BOLD)
        self.app_name.SetFont(self.app_name_font)

        self.show_planner()
        self.create_io_textbox()
        self.create_button()
        self.create_toolbar()
        self.initStatusBar() 
        self.createMenuBar()



    def show_planner(self):
        #Listbox
        sap = wx.StaticText(self.panel, -1, "Select a planner:", (20,150))
        sap.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.BOLD))
        self.PlannerList = ['IPP', 'HSP', 'FF', 'Graphplan']
        self.listBox = wx.Choice(self.panel, -1, (200, 145), choices=self.PlannerList)
        self.planner_init()
        self.Bind(wx.EVT_CHOICE, self.choice_init, self.listBox)

        #Chose domain file and problem file
        cad = wx.StaticText(self.panel, -1, "Chose a domain:", (20,480), style=wx.ALIGN_LEFT)
        cap = wx.StaticText(self.panel, -1, "Chose a problem:", (20,520), style=wx.ALIGN_LEFT)
        cad.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.BOLD))
        cap.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.BOLD))
        self.dom_button = wx.Button(self.panel, -1,"Chose", pos=(150, 475), size=(180, -1))  
        self.Bind(wx.EVT_BUTTON, self.Domain_Button, self.dom_button)
        #self.dom_button.SetDefault()
        self.pro_button = wx.Button(self.panel, -1,"Chose", pos=(150, 515), size=(180, -1))  
        self.Bind(wx.EVT_BUTTON, self.Problem_Button, self.pro_button)
        #self.pro_button.SetDefault()


    def create_io_textbox(self):
        #Input textbox
        ofn = wx.StaticText(self.panel, -1, "Output file name:", (20,560), style=wx.ALIGN_LEFT)
        ofn.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.BOLD))
        self.out_file_name = wx.TextCtrl(self.panel, -1, "", size=(180, -1), 
            pos=(150,555), style=wx.TE_PROCESS_ENTER)
        self.out_file_name.SetInsertionPoint(0)

        #Output textbox
        output_label = wx.StaticText(self.panel, -1, "Realtime output", (440,20), (440,60), wx.ALIGN_CENTER)
        output_label_font = wx.Font(18, wx.SCRIPT, wx.NORMAL, wx.BOLD)
        output_label.SetFont(output_label_font)
        self.OutputText = wx.TextCtrl(self.panel, -1,"",
        size=(640, 560), pos=(340,60), style=wx.TE_MULTILINE | wx.TE_READONLY | wx.TE_RICH2) 
        output_font = wx.Font(self.out_size, self.out_style, wx.NORMAL, wx.NORMAL)
        self.OutputText.SetFont(output_font)
        self.pointer = self.OutputText.GetInsertionPoint()


    def create_button(self):
        self.button1 = wx.Button(self.panel, -1,"Test", pos=(100, 660))  
        self.Bind(wx.EVT_BUTTON, self.Test_Button, self.button1)
        #self.button1.SetDefault()

        self.button2 = wx.Button(self.panel, -1,"Run", pos=(200, 660))  
        self.Bind(wx.EVT_BUTTON, self.Run_Button, self.button2)
        #self.button2.SetDefault()

        self.button3 = wx.Button(self.panel, -1,"Clear", pos=(300, 660))  
        self.Bind(wx.EVT_BUTTON, self.Clear_Button, self.button3)
        #self.button3.SetDefault()

        self.button6 = wx.Button(self.panel, -1,"Exit", pos=(400, 660))  
        self.Bind(wx.EVT_BUTTON, self.Exit_Button, self.button6)
        #self.button6.SetDefault()


    def create_toolbar(self): 
        pass
        '''         
        toolbar = self.CreateToolBar()  
        toolbar.AddLabelTool(wx.ID_ANY, 'Open', wx.Bitmap("icons/open.png"))  
        toolbar.AddLabelTool(wx.ID_ANY, 'New', wx.Bitmap("icons/new.png"))  
        toolbar.AddLabelTool(wx.ID_ANY, 'Save', wx.Bitmap("icons/save.png"))  
        toolbar.AddLabelTool(wx.ID_EXIT, '', wx.Bitmap("icons/exit.png"))  
        toolbar.Realize() ''' 


    def initStatusBar(self):
        self.statusBar = self.CreateStatusBar()
        self.statusBar.SetFieldsCount(3)
        self.statusBar.SetStatusWidths([-1, -2, -3])


    def menuData(self):
        return [("&File", 
                    (
                        ("&New", "New a file", self.OnNew),
                        ("&Open", "Open a file", self.OnOpen),
                        ("&Save", "Save a file", self.OnSave),
                        ("&Quit", "Quit", self.OnCloseWindow),
                        ("", "", "")
                    )
                ),
                ("&Preferences",
                    (
                        ("&Backgound Color", 
                            (
                                ("&Default", "", self.OnColor, wx.ITEM_RADIO),
                                ("&aquamarine", "", self.OnColor, wx.ITEM_RADIO),
                                ("&black", "", self.OnColor, wx.ITEM_RADIO),
                                ("&blue", "", self.OnColor, wx.ITEM_RADIO),
                                ("&blue violet", "", self.OnColor, wx.ITEM_RADIO),
                                ("&brown", "", self.OnColor, wx.ITEM_RADIO),
                                ("&cadet blue", "", self.OnColor, wx.ITEM_RADIO),
                                ("&coral", "", self.OnColor, wx.ITEM_RADIO),
                                ("&cornflower blue", "", self.OnColor, wx.ITEM_RADIO),
                                ("&cyan", "", self.OnColor, wx.ITEM_RADIO),
                                ("&dark gray", "", self.OnColor, wx.ITEM_RADIO),
                                ("&dark green:", "", self.OnColor, wx.ITEM_RADIO),
                                ("&dark olive green", "", self.OnColor, wx.ITEM_RADIO),
                                ("&dark orchid", "", self.OnColor, wx.ITEM_RADIO),
                                ("&dark slate blue", "", self.OnColor, wx.ITEM_RADIO),
                                ("&dark slate gray", "", self.OnColor, wx.ITEM_RADIO),
                                ("&dark turquoise", "", self.OnColor, wx.ITEM_RADIO),
                                ("&dim gray", "", self.OnColor, wx.ITEM_RADIO),
                                ("&firebrick", "", self.OnColor, wx.ITEM_RADIO),
                                ("&forest green", "", self.OnColor, wx.ITEM_RADIO),
                                ("&gold", "", self.OnColor, wx.ITEM_RADIO),
                                ("&goldnrod", "", self.OnColor, wx.ITEM_RADIO),
                                ("&gray", "", self.OnColor, wx.ITEM_RADIO),
                                ("&green", "", self.OnColor, wx.ITEM_RADIO),
                                ("&green yellow", "", self.OnColor, wx.ITEM_RADIO),
                                ("&indian red", "", self.OnColor, wx.ITEM_RADIO),
                                ("&khaki", "", self.OnColor, wx.ITEM_RADIO),
                                ("&light blue", "", self.OnColor, wx.ITEM_RADIO),
                                ("&light gray", "", self.OnColor, wx.ITEM_RADIO),
                                ("&light steel blue", "", self.OnColor, wx.ITEM_RADIO),
                                ("&lime green", "", self.OnColor, wx.ITEM_RADIO),
                                ("&magenta", "", self.OnColor, wx.ITEM_RADIO),
                                ("&maroon", "", self.OnColor, wx.ITEM_RADIO),
                                ("&medium aquamarine", "", self.OnColor, wx.ITEM_RADIO),
                                ("&medium blue", "", self.OnColor, wx.ITEM_RADIO),
                                ("&medium forest green", "", self.OnColor, wx.ITEM_RADIO),
                                ("&medium goldnrod", "", self.OnColor, wx.ITEM_RADIO),
                                ("&medium orchid", "", self.OnColor, wx.ITEM_RADIO),
                                ("&medium sea green", "", self.OnColor, wx.ITEM_RADIO),
                                ("&medium slate blue", "", self.OnColor, wx.ITEM_RADIO),
                                ("&medium spring green", "", self.OnColor, wx.ITEM_RADIO),
                                ("&medium turquoise", "", self.OnColor, wx.ITEM_RADIO),
                                ("&medium violet red", "", self.OnColor, wx.ITEM_RADIO),
                                ("&midnight blue", "", self.OnColor, wx.ITEM_RADIO),
                                ("&navy", "", self.OnColor, wx.ITEM_RADIO),
                                ("&orange", "", self.OnColor, wx.ITEM_RADIO),
                                ("&orange red", "", self.OnColor, wx.ITEM_RADIO),
                                ("&orchid", "", self.OnColor, wx.ITEM_RADIO),
                                ("&pale green", "", self.OnColor, wx.ITEM_RADIO),
                                ("&pink", "", self.OnColor, wx.ITEM_RADIO),
                                ("&plum", "", self.OnColor, wx.ITEM_RADIO),
                                ("&purple", "", self.OnColor, wx.ITEM_RADIO),
                                ("&red", "", self.OnColor, wx.ITEM_RADIO),
                                ("&salmon", "", self.OnColor, wx.ITEM_RADIO),
                                ("&sea green", "", self.OnColor, wx.ITEM_RADIO),
                                ("&sienna", "", self.OnColor, wx.ITEM_RADIO),
                                ("&sky blue", "", self.OnColor, wx.ITEM_RADIO),
                                ("&slate blue", "", self.OnColor, wx.ITEM_RADIO),
                                ("&spring green", "", self.OnColor, wx.ITEM_RADIO),
                                ("&steel blue", "", self.OnColor, wx.ITEM_RADIO),
                                ("&tan", "", self.OnColor, wx.ITEM_RADIO),
                                ("&thistle", "", self.OnColor, wx.ITEM_RADIO),
                                ("&turquoise", "", self.OnColor, wx.ITEM_RADIO),
                                ("&violet", "", self.OnColor, wx.ITEM_RADIO),
                                ("&violet red", "", self.OnColor, wx.ITEM_RADIO),
                                ("&wheat", "", self.OnColor, wx.ITEM_RADIO),
                                ("&white", "", self.OnColor, wx.ITEM_RADIO),
                                ("&yellow", "", self.OnColor, wx.ITEM_RADIO),
                                ("&yellow green", "", self.OnColor, wx.ITEM_RADIO),
                                ("", "", "")
                            )
                        ),
                        ("&Font Style", 
                            (
                                ("&DEFAULT", "", self.OutputFontStyle, wx.ITEM_RADIO),
                                ("&DECORATIVE",    "", self.OutputFontStyle, wx.ITEM_RADIO),
                                ("&MODERN",     "", self.OutputFontStyle, wx.ITEM_RADIO),
                                ("&ROMAN",      "", self.OutputFontStyle, wx.ITEM_RADIO),
                                ("&SCRIPT",     "", self.OutputFontStyle, wx.ITEM_RADIO),
                                ("&SWISS",      "", self.OutputFontStyle, wx.ITEM_RADIO),
                                ("", "", "")
                            )
                        ),
                        ("&Font Size", 
                            (
                                ("&1",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&2",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&3",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&4",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&5",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&6",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&7",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&8",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&9",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&10",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&11",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&12",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&13",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&14",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&15",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&16",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&17",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&18",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&19",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&20",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&21",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&22",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&23",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&24",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&25",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&26",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&27",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&28",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&29",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&30",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&31",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("&32",  "", self.OutputFontSize, wx.ITEM_RADIO),
                                ("", "", "")
                            )
                        )
                    )
                )]


    def createMenuBar(self):
        self.menuBar = wx.MenuBar()
        for eachMenuData in self.menuData():
            menuLabel = eachMenuData[0]
            menuItems = eachMenuData[1]
            self.menuBar.Append(self.createMenu(menuItems), menuLabel)
        self.SetMenuBar(self.menuBar)


    def createMenu(self, menuData):
        menu = wx.Menu()
        for eachItem in menuData:
            if len(eachItem) == 2:
                label = eachItem[0]
                subMenu = self.createMenu(eachItem[1])
                menu.AppendMenu(wx.NewId(), label, subMenu)
            else:
                self.createMenuItem(menu, *eachItem)
        return menu


    def createMenuItem(self, menu, label, status, handler, kind=wx.ITEM_NORMAL):
        if not label:
            menu.AppendSeparator()
            return
        menuItem = menu.Append(-1, label, status, kind)
        self.Bind(wx.EVT_MENU, handler, menuItem)


    def OnNew(self, event): pass
    def OnOpen(self, event): pass
    def OnSave(self, event): pass
    def OnColor(self, event): 
        menubar = self.GetMenuBar()
        itemId = event.GetId()
        item = menubar.FindItemById(itemId)
        color = item.GetLabel()
        self.panel.SetBackgroundColour(color)
        self.panel.Refresh()


    def OutputFontStyle(self, event):
        menubar = self.GetMenuBar()
        itemId = event.GetId()
        item = menubar.FindItemById(itemId)
        font_style = item.GetLabel()
        if font_style == 'DECORATIVE':
            self.out_style = wx.DECORATIVE
        elif font_style == 'MODERN':
            self.out_style = wx.MODERN
        elif font_style == 'ROMAN':
            self.out_style = wx.ROMAN
        elif font_style == 'SCRIPT':
            self.out_style = wx.SCRIPT
        elif font_style == 'SWISS':
            self.out_style = wx.SWISS
        else:
            self.out_style = wx.DEFAULT
        self.OutputText.SetFont(wx.Font(self.out_size, self.out_style, wx.NORMAL, wx.NORMAL))
        self.OutputText.AppendText('Output text font is set to %s\n'%font_style)


    def OutputFontSize(self, event):
        menubar = self.GetMenuBar()
        itemId = event.GetId()
        item = menubar.FindItemById(itemId)
        change = item.GetLabel()
        self.out_size = int(change)
        self.OutputText.SetFont(wx.Font(self.out_size, self.out_style, wx.NORMAL, wx.NORMAL))
        self.OutputText.AppendText('Output text font size is set to %s\n'%self.out_size)


    def OnCloseWindow(self, event): pass


    def planner_init(self):
        self.hsp_a_text = 0
        self.hsp_d_text = 0 
        self.hsp_h_text = 0
        self.ff_info_text = 0
        self.ipp_info_text = 0
        self.gp_info_text = 0
        self.gp_default_text = 0


    def choice_init(self, event):
        p = self.listBox.GetSelection()
        if p == 0:
            if self.hsp_a_text:
                self.__hsp_destroy()
            elif self.ff_info_text:
                self.__ff_destroy()
            elif self.gp_default_text:
                self.__graphplan_destroy()
            self.__ipp_show()
        elif p == 1:
            if self.ipp_info_text:
                self.__ipp_destroy()
            elif self.ff_info_text:
                self.__ff_destroy()
            elif self.gp_default_text:
                self.__graphplan_destroy()
            self.__hsp_show()
        elif p == 2:
            if self.ipp_info_text:
                self.__ipp_destroy()
            elif self.hsp_a_text:
                self.__hsp_destroy()
            elif self.gp_default_text:
                self.__graphplan_destroy()
            self.__ff_show()
        elif p == 3:
            if self.ipp_info_text:
                self.__ipp_destroy()
            elif self.hsp_a_text:
                self.__hsp_destroy()
            elif self.ff_info_text:
                self.__ff_destroy()
            self.__graphplan_show()
        else:
            pass
            

    def __ipp_show(self):
        self.ipp_info_list = [str(i) for i in range(9)]
        self.ipp_info_text = wx.StaticText(self.panel, -1, "Information level:", (20,190), style=wx.ALIGN_LEFT)
        self.ipp_info_text.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.BOLD))
        self.ipp_info_choice = wx.Choice(self.panel, -1, (200, 185), choices=self.ipp_info_list)
        self.__ipp_help()


    def __ipp_help(self):
        self.pointer = self.OutputText.GetInsertionPoint()
        self.OutputText.AppendText("\n\nusage of ipp:\n\n");
        self.OutputText.AppendText("OPTIONS   DESCRIPTIONS\n\n");
        self.OutputText.AppendText("-p <str>    path for operator and fact file\n");
        self.OutputText.AppendText("-o <str>    operator file name\n");
        self.OutputText.AppendText("-f <str>    fact file name\n\n");

        self.OutputText.AppendText("-i <num>    run-time information level( preset: 1 )\n");
        self.OutputText.AppendText("     0      nothing\n");
        self.OutputText.AppendText("     1      info on action number, graph, search and plan\n");
        self.OutputText.AppendText("     2      1 + info on problem constants, types and predicates\n");  
        self.OutputText.AppendText("     3      1 + 2 + loaded operators, initial and goal state\n");
        self.OutputText.AppendText("     4      1 + predicates and their inertia status\n");
        self.OutputText.AppendText("     5      1 + 4 + goal state and operators with unary inertia encoded\n");
        self.OutputText.AppendText("     6      1 + actions, initial and goal state after expansion of variables\n"); 
        self.OutputText.AppendText("     7      1 + facts selected as relevant to the problem\n");
        self.OutputText.AppendText("     8      1 + final domain representation\n");
        self.OutputText.AppendText(" > 100      1 + various debugging information\n\n");

        self.OutputText.AppendText("-W          write complete graph to text files after planning\n");
        self.OutputText.AppendText("-w <str>    specify name for graph output files( preset: graph )\n\n");

        self.OutputText.AppendText("-m <num>    build graph up to level <num> without search\n");
        self.OutputText.AppendText("-S          don't do complete subset test in memoization\n\n");
        tp_point = self.pointer
        self.pointer = self.OutputText.GetInsertionPoint()
        self.OutputText.SetStyle(tp_point, self.pointer, wx.TextAttr("sienna", wx.NullColour))


    def __ipp_destroy(self):
        self.ipp_info_text.Destroy()
        self.ipp_info_choice.Destroy()


    def __hsp_show(self):
        self.HSP_algorithm = ['bfs', 'gbfs']
        self.HSP_direction = ['forward', 'backward']
        self.HSP_heuristic = ['h1max', 'h2plus', 'h1plus']
        self.hsp_a_text = wx.StaticText(self.panel, -1, "Algorithm:", (20,190), style=wx.ALIGN_LEFT)
        self.hsp_d_text = wx.StaticText(self.panel, -1, "Direction:", (20,230), style=wx.ALIGN_LEFT)
        self.hsp_h_text = wx.StaticText(self.panel, -1, "Heuristic:", (20,270), style=wx.ALIGN_LEFT)
        self.hsp_a_text.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.BOLD))
        self.hsp_d_text.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.BOLD))
        self.hsp_h_text.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.BOLD))
        self.hsp_a_choice = wx.Choice(self.panel, -1, (200, 185), choices=self.HSP_algorithm)
        self.hsp_d_choice = wx.Choice(self.panel, -1, (200, 225), choices=self.HSP_direction)
        self.hsp_h_choice = wx.Choice(self.panel, -1, (200, 265), choices=self.HSP_heuristic)
        self.__hsp_help()


    def __hsp_help(self):
        self.pointer = self.OutputText.GetInsertionPoint()
        self.OutputText.AppendText("\n\nusage of hsp:\n\n");
        self.OutputText.AppendText( "hsp <flags>* [ <algorithm> | -S <schedule> ] <problem.pddl> <domain.pddl>\n\n" );
        self.OutputText.AppendText( "where <flags> are among:\n" );
        self.OutputText.AppendText( "   -v <level>\t\tVerbose level >= 0 (default is 1).\n" );
        self.OutputText.AppendText( "    r <output_file>\t\tdefault is 'hsp_output.txt'.\n" );
        self.OutputText.AppendText( "   -w <weight>\t\tFloat to weight the heuristic component of the cost function.\n\n" );
        self.OutputText.AppendText( "<algorithm> is:\n" );
        self.OutputText.AppendText( "   -a <algorithm>\tEither 'bfs' or 'gbfs'.\n" );
        self.OutputText.AppendText( "   -d <direction>\tEither 'forward' or 'backward'.\n" );
        self.OutputText.AppendText( "   -h <heuristic>\tOne of 'h1plus', 'h1max', 'h2plus', 'h2max'.\n\n" );
        self.OutputText.AppendText( "<schedule> is a colon separated <option> list where each option has\n" );
        self.OutputText.AppendText( "form '[<direction>,<heuristic>,<msecs>]'. The options are performed\n" );
        self.OutputText.AppendText( "sequentially until one finds a plan, or no more options are available\n" );
        self.OutputText.AppendText( "(each option is attempted with the given time constraint).\n\n" );
        tp_point = self.pointer
        self.pointer = self.OutputText.GetInsertionPoint()
        self.OutputText.SetStyle(tp_point, self.pointer, wx.TextAttr("coral", wx.NullColour))


    def __hsp_destroy(self):
        self.hsp_a_text.Destroy()
        self.hsp_d_text.Destroy()
        self.hsp_h_text.Destroy()
        self.hsp_a_choice.Destroy()
        self.hsp_d_choice.Destroy()
        self.hsp_h_choice.Destroy()


    def __ff_show(self):
        a = range(3)
        b = range(101, 128)
        a.extend(b)
        self.ff_info_list = [str(i) for i in a]
        self.ff_info_text = wx.StaticText(self.panel, -1, "Information level:", 
            (20,190), style=wx.ALIGN_LEFT)
        self.ff_info_text.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.BOLD))
        self.ff_info_choice = wx.Choice(self.panel, -1, (200, 185), 
            choices=self.ff_info_list)
        self.__ff_help()


    def __ff_help(self):
        self.pointer = self.OutputText.GetInsertionPoint()
        self.OutputText.AppendText("\n\nusage of ff:\n");
        self.OutputText.AppendText("\nOPTIONS   DESCRIPTIONS\n\n");
        self.OutputText.AppendText("-p <str>    path for operator and fact file\n");
        self.OutputText.AppendText("-o <str>    operator file name\n");
        self.OutputText.AppendText("-f <str>    fact file name\n\n");
        self.OutputText.AppendText("-i <num>    run-time information level( preset: 1 )\n");
        self.OutputText.AppendText("      0     only times\n");
        self.OutputText.AppendText("      1     problem name, planning process infos\n");
        self.OutputText.AppendText("      2     save pure output action sequences in file\n");
        self.OutputText.AppendText("    101     parsed problem data\n");
        self.OutputText.AppendText("    102     cleaned up ADL problem\n");
        self.OutputText.AppendText("    103     collected string tables\n");
        self.OutputText.AppendText("    104     encoded domain\n");
        self.OutputText.AppendText("    105     predicates inertia info\n");
        self.OutputText.AppendText("    106     splitted initial state\n");
        self.OutputText.AppendText("    107     domain with Wff s normalized\n");
        self.OutputText.AppendText("    108     domain with NOT conds translated\n");
        self.OutputText.AppendText("    109     splitted domain\n");
        self.OutputText.AppendText("    110     cleaned up easy domain\n");
        self.OutputText.AppendText("    111     unaries encoded easy domain\n");
        self.OutputText.AppendText("    112     effects multiplied easy domain\n");
        self.OutputText.AppendText("    113     inertia removed easy domain\n");
        self.OutputText.AppendText("    114     easy action templates\n");
        self.OutputText.AppendText("    115     cleaned up hard domain representation\n");
        self.OutputText.AppendText("    116     mixed hard domain representation\n");
        self.OutputText.AppendText("    117     final hard domain representation\n");
        self.OutputText.AppendText("    118     reachability analysis results\n");
        self.OutputText.AppendText("    119     facts selected as relevant\n");
        self.OutputText.AppendText("    120     final domain and problem representations\n");
        self.OutputText.AppendText("    121     connectivity graph\n");
        self.OutputText.AppendText("    122     fixpoint result on each evaluated state\n");
        self.OutputText.AppendText("    123     1P extracted on each evaluated state\n");
        self.OutputText.AppendText("    124     H set collected for each evaluated state\n");
        self.OutputText.AppendText("    125     False sets of goals <GAM>\n");
        self.OutputText.AppendText("    126     detected ordering constraints leq_h <GAM>\n");
        self.OutputText.AppendText("    127     the Goal Agenda <GAM>\n");
        tp_point = self.pointer
        self.pointer = self.OutputText.GetInsertionPoint()
        self.OutputText.SetStyle(tp_point, self.pointer, wx.TextAttr("blue", wx.NullColour))


    def __ff_destroy(self):
        self.ff_info_text.Destroy()
        self.ff_info_choice.Destroy()


    def __graphplan_show(self):
        self.gp_default_list = ['False', 'True']
        self.gp_default_text = wx.StaticText(self.panel, -1, 
            "Default setting:", (20,190), style=wx.ALIGN_LEFT)
        self.gp_default_text.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.BOLD))
        self.gp_default_choice = wx.Choice(self.panel, -1, (200, 185), 
            choices=self.gp_default_list)
        self.Bind(wx.EVT_CHOICE, self.__graphplan_setting, self.gp_default_choice)
        self.__graphplan_help()


    def __graphplan_help(self):
        self.pointer = self.OutputText.GetInsertionPoint()
        self.OutputText.AppendText("\n\nusage of graphplan:\n");
        self.OutputText.AppendText("\nOPTIONS   DESCRIPTIONS\n\n");
        self.OutputText.AppendText("-h          for this list\n");
        self.OutputText.AppendText("-o <str>    operator file name\n");
        self.OutputText.AppendText("-f <str>    fact file name\n\n");
        self.OutputText.AppendText("-t <num>    to specify a fixed number of time steps\n");
        self.OutputText.AppendText("-i <num>    to specify info level 1 or 2 (default is 0)\n\n");
        self.OutputText.AppendText("-O <str>    to specify options you want\n");
        self.OutputText.AppendText("-d          give default values to everything not specified\n\n");
        self.OutputText.AppendText("for example: graphplan -o fixit_ops -f fixit_facts1 -O IL -d \n\n");
        tp_point = self.pointer
        self.pointer = self.OutputText.GetInsertionPoint()
        self.OutputText.SetStyle(tp_point, self.pointer, wx.TextAttr("green", wx.NullColour))


    def __graphplan_setting(self, event):
        d = self.gp_default_choice.GetSelection()
        if d == 0:
            self.gp_info_list = [str(i) for i in range(3)]
            self.gp_info_text = wx.StaticText(self.panel, -1, 
                "Information level:", (20,230), style=wx.ALIGN_LEFT)
            self.gp_info_text.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.BOLD))
            self.gp_info_choice = wx.Choice(self.panel, -1, (200, 225), 
                choices=self.gp_info_list)

            self.gp_time_steps_text = wx.StaticText(self.panel, -1, 
                "Time steps:", (20,270), style=wx.ALIGN_LEFT)
            self.gp_time_steps_text.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.BOLD))
            self.gp_time_steps = wx.TextCtrl(self.panel, -1, "", size=(120, -1), 
                pos=(200,265), style=wx.TE_PROCESS_ENTER)
            self.gp_time_steps.SetInsertionPoint(0)

            self.gp_options_text = wx.StaticText(self.panel, -1, 
                "Options:", (20,310), style=wx.ALIGN_LEFT)
            self.gp_options_text.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.BOLD))
            self.gp_options_list = ['show irrelevants', 'Lower bound time', 
            'Build up to goals', 'no mutual exclusion', 'examine subsets']
            self.gp_op_list = ['I', 'L', 'B', 'E', 'S']
            self.gp_options_choices = wx.CheckListBox(self.panel, -1, (150,300),
                (180,120), self.gp_options_list, wx.LB_MULTIPLE)
        else:
            if self.gp_info_text:
                self.gp_info_text.Destroy()
                self.gp_info_choice.Destroy()
                self.gp_time_steps_text.Destroy()
                self.gp_time_steps.Destroy()
                self.gp_options_text.Destroy()
                self.gp_options_choices.Destroy()
            

    def __graphplan_destroy(self):
        self.gp_default_text.Destroy()
        self.gp_default_choice.Destroy()
        if self.gp_info_text:
            self.gp_info_text.Destroy()
            self.gp_info_choice.Destroy()
            self.gp_time_steps_text.Destroy()
            self.gp_time_steps.Destroy()
            self.gp_options_text.Destroy()
            self.gp_options_choices.Destroy()



    def Domain_Button(self, event):
        dom_type = "PDDL source (*.pddl)|*.pddl|" \
        "Text source (*.txt)|*.txt|" \
        "All files (*.*)|*.*"
        dialog = wx.FileDialog(None, "Choose a file", self.pddldir, "", 
            dom_type, wx.OPEN)
        if dialog.ShowModal() == wx.ID_OK:
            self.dom_file = dialog.GetPath().encode('utf-8') 
            self.dom_button.SetLabelText(re.sub(r'.*/','',self.dom_file))
        dialog.Destroy()


    def Problem_Button(self, event):
        pro_type = "PDDL source (*.pddl)|*.pddl|" \
        "Text source (*.txt)|*.txt|" \
        "All files (*.*)|*.*"
        dialog = wx.FileDialog(None, "Choose a file", self.pddldir, "", 
            pro_type, wx.OPEN)
        if dialog.ShowModal() == wx.ID_OK:
            self.pro_file = dialog.GetPath().encode('utf-8') 
            self.pro_button.SetLabelText(re.sub(r'.*/','',self.pro_file)) 
        dialog.Destroy()


    def Test_Button(self, event):
        self.OutputText.AppendText("\nTest_Button\n")
        print 'testing...'
        self.count += 1
        self.statusBar.SetStatusText('tesing %d'%self.count, self.count%3)


    def Run_Button(self, event):
        ind = self.listBox.GetSelection()
        tool_name = self.PlannerList[ind]
        self.OutputText.AppendText("\nUsing planner %s...\n"%tool_name)

        pl = self.PlannerList[ind].lower()
        temp = self.out_file_name.GetValue().encode('utf-8').split()
        self.out_file_name.Clear()

        lib = cdll.LoadLibrary('./libs/%s.so'%pl)
        if pl == 'ipp':
            num = self.ipp_info_choice.GetSelection()
            if temp:
                argv = ['ipp', '-o', self.dom_file, '-f', self.pro_file, '-r', 
                temp[0], '-i', self.ipp_info_list[num]]
            else:
                argv = ['ipp', '-o', self.dom_file, '-f', self.pro_file, '-i', 
                self.ipp_info_list[num]]
        elif pl == 'hsp':
            a = self.hsp_a_choice.GetSelection()
            d = self.hsp_d_choice.GetSelection()
            h = self.hsp_h_choice.GetSelection()
            if temp:
                argv = ['hsp', 'r', temp[0], '-a', self.HSP_algorithm[a], '-d', 
                self.HSP_direction[d], '-h', self.HSP_heuristic[h], self.pro_file, self.dom_file ]
            else:
                argv = ['hsp', '-a', self.HSP_algorithm[a], '-d', self.HSP_direction[d], 
                '-h', self.HSP_heuristic[h], self.pro_file, self.dom_file ]
            print argv
        elif pl == 'ff':
            num = self.ff_info_choice.GetSelection()
            if temp:
                argv = ['ff', '-o', self.dom_file, '-f', self.pro_file, '-r', 
                temp[0], '-i', self.ff_info_list[num]]
            else:
                argv = ['ff', '-o', self.dom_file, '-f', self.pro_file, '-i', 
                self.ff_info_list[num]]
        elif pl == 'graphplan':       
            d = self.gp_default_choice.GetSelection()
            if d != 0:
                if temp:
                    argv = ['graphplan', '-r', temp[0], '-o', self.dom_file, '-f', self.pro_file, '-d']
                else:
                    argv = ['graphplan', '-o', self.dom_file, '-f', self.pro_file, '-d']
            else:
                num = self.gp_info_choice.GetSelection()   
                ops = ''
                for i in range(len(self.gp_options_list)):
                    if self.gp_options_choices.IsChecked(i):
                        ops += self.gp_op_list[i]
                temp_time_steps = self.gp_time_steps.GetValue().encode('utf-8').split()
                if not temp_time_steps:
                    ts = '0'
                else:
                    ts = temp_time_steps[0]
                if temp:
                    argv = ['graphplan', '-r', temp[0], '-o', self.dom_file, '-f', self.pro_file, 
                    '-i', self.gp_info_list[num], '-t', ts, '-O', ops]
                else:
                    argv = ['graphplan', '-o', self.dom_file, '-f', self.pro_file, 
                    '-i', self.gp_info_list[num], '-t', ts, '-O', ops]


        self.OutputText.AppendText('\nInput argv is %s\n'%' '.join(argv))
        self.OutputText.AppendText('\nDoing planning for your problem...')

        lib.main(len(argv), (c_char_p*len(argv))(*argv))
        if temp:
            self.OutputText.AppendText(
                '\nThe planning is done!\nResult of it is saved in %s\n'%temp[0])
        else:
            self.OutputText.AppendText(
                '\nThe planning is done!\nResult of it is saved in %s_output.txt\n'%pl)
            

    def Clear_Button(self, event): 
        self.OutputText.Clear()


    def Exit_Button(self, event): 
        self.Destroy()


    def OnText(self, event): pass




class MainWindow(wx.Frame):
    def __init__(self):
        wx.Frame.__init__(self, None, -1, "StaticBoxSizer Test")
        self.panel = wx.Panel(self)

        self.display_init()
        self.applications_init()
        self.toolbar_init()

        content_box = wx.BoxSizer(wx.HORIZONTAL)
        content_box.Add(self.display_box, 0, wx.ALL, 5)
        content_box.Add(self.button_box, 0, wx.ALL, 5)
         
        main_box = wx.BoxSizer(wx.VERTICAL)
        main_box.Add(self.tool_box, 0, wx.ALL, 0)
        main_box.Add(content_box, 0, wx.ALL, 0)

        self.panel.SetSizer(main_box)
        main_box.Fit(self)


    def display_init(self):
        img1 = wx.Image('icons/planning2.jpg', wx.BITMAP_TYPE_ANY)
        w = img1.GetWidth()
        h = img1.GetHeight()
        img2 = img1.Scale(w, h)
        logo = wx.StaticBitmap(self.panel, -1, wx.BitmapFromImage(img2))

        app_name = wx.StaticText(self.panel, -1, 'PlanTool')
        app_name.SetFont(wx.Font(24, wx.SCRIPT, wx.NORMAL, wx.BOLD))
        app_info = wx.StaticText(self.panel, -1, 
            '''
            An integrated environment for planning 
            Version 1.0
            All rights reserved (c) PlanLab 2017''')
        app_info.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.NORMAL))
        self.display_box = wx.BoxSizer(wx.VERTICAL)
        self.display_box.Add(logo, 0, wx.EXPAND|wx.ALL, 5)
        self.display_box.Add(app_name, 0, wx.ALIGN_CENTER|wx.ALL, 5)
        self.display_box.Add(app_info, 0, wx.ALL, 5)


    def applications_init(self):
        det_but = buttons.GenButton(self.panel, -1, 'Deterministic Track')
        unc_but = buttons.GenButton(self.panel, -1, 'Uncertainty Track')
        lrn_but = buttons.GenButton(self.panel, -1, 'Learning Track')
        self.Bind(wx.EVT_BUTTON, self.DeterministicTrack, det_but)
        self.Bind(wx.EVT_BUTTON, self.UncertaintyTrack, unc_but)
        self.Bind(wx.EVT_BUTTON, self.LearningTrack, lrn_but)
        box = wx.StaticBox(self.panel, -1, 'Applications')
        self.button_box = wx.StaticBoxSizer(box, wx.VERTICAL)
        self.button_box.Add(det_but, 0, wx.EXPAND|wx.ALL, 2)
        self.button_box.Add(unc_but, 0, wx.EXPAND|wx.ALL, 2)
        self.button_box.Add(lrn_but, 0, wx.EXPAND|wx.ALL, 2)


    def set_gen_button(self, but_name, fs=8, fsty=wx.NORMAL, fity=wx.NORMAL, 
        fbold=wx.NORMAL, bw=0, bcolor='DEFAULT', fcolor='black', 
        tips=''):
        but_name.SetFont(wx.Font(fs, fsty, fity, fbold, False))
        but_name.SetBezelWidth(bw)
        but_name.SetBackgroundColour(bcolor)
        but_name.SetForegroundColour(fcolor)
        but_name.SetToolTipString(tips)


    def toolbar_init(self):
        fs = 8
        bw = 0
        fsty = wx.SWISS
        fity = wx.NORMAL
        fbold = wx.NORMAL
        bcolor = 'gray'
        fcolor = 'white'
        self.tool_box = wx.BoxSizer(wx.HORIZONTAL)
        set_but = buttons.GenButton(self.panel, -1, 'Setting', size=(60,20))
        help_but = buttons.GenButton(self.panel, -1, 'Help', size=(60,20))
        about_but = buttons.GenButton(self.panel, -1, 'About', size=(60,20))
        exit_but = buttons.GenButton(self.panel, -1, 'Exit', size=(60,20))
        self.set_gen_button(set_but, fs, fsty, fity, fbold, bw, bcolor, fcolor, 
            'Setting for font and color')
        self.set_gen_button(help_but, fs, fsty, fity, fbold, bw, bcolor, fcolor, 
            'Help for the PlanTool')
        self.set_gen_button(about_but, fs, fsty, fity, fbold, bw, bcolor, fcolor, 
            'More information about our team')
        self.set_gen_button(exit_but, fs, fsty, fity, fbold, bw, bcolor, fcolor, 
            'Exit this app')
        self.Bind(wx.EVT_BUTTON, self.OnSetting, set_but)
        self.Bind(wx.EVT_BUTTON, self.OnHelp, help_but)
        self.Bind(wx.EVT_BUTTON, self.OnAbout, about_but)
        self.Bind(wx.EVT_BUTTON, self.OnExit, exit_but)
        self.tool_box.Add(set_but, 0, wx.ALL, 0)
        self.tool_box.Add(help_but, 0, wx.ALL, 0)
        self.tool_box.Add(about_but, 0, wx.ALL, 0)
        self.tool_box.Add(exit_but, 0, wx.ALL, 0)


    def DeterministicTrack(self, event):
        self.child_frame = PlannerGUI()
        self.child_frame.Show()

    def UncertaintyTrack(self, event): pass
    def LearningTrack(self, event): pass
    def OnSetting(self, event): 
        dlg = wx.MessageBox('This function is under construction.', 'Tips for Setting',
            wx.OK|wx.ICON_INFORMATION)

    def OnHelp(self, event): 
        dlg = wx.MessageBox('This function is under construction.', 'Tips for Help',
            wx.OK|wx.ICON_INFORMATION)


    def OnAbout(self, event):
        dlg = PlanToolAbout(self)
        dlg.ShowModal()
        dlg.Destroy()

    def OnExit(self, event):
        self.Destroy()


class PlanToolAbout(wx.Dialog):
    def __init__(self, parent):
        wx.Dialog.__init__(self, parent, -1, 'About PlanLab', size=(440, 400) )
        html = wx.html.HtmlWindow(self)
        self.text = '''
        <html>
        <body bgcolor="#ACAA60">
        <center><table bgcolor="#455481" width="100%" cellspacing="0"
        cellpadding="0" border="1">
        <tr>
            <td align="center"><h1>PlanLab!</h1></td>
        </tr>
        </table>
        </center>
        <p><b>PlanLab</b> is a lab of the shcool of data and computer science of 
        Sun Yet-sen University, Guangzhou, China. Our major themes in research are
        Planning, Learning, Agent and NLP (<b>PLAN</b>).
        </p>
        <p>Lab Advisor: <b>Hankz Hankui Zhuo </b></p>
        <p>Researcher: <b>Huiling Zhu </b></p>
        <p>Current Students: <b>17 members </b></p>
        <p> Copyright (c) 2012-2017</p>
        </body>
        </html>
        '''
        html.SetPage(self.text)
        button = wx.Button(self, wx.ID_OK, "Okay")
        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer.Add(html, 1, wx.EXPAND|wx.ALL, 5)
        sizer.Add(button, 0, wx.ALIGN_CENTER|wx.ALL, 5)
        self.SetSizer(sizer)
        self.Layout()


class MyApp(wx.App):
    def __init__(self, redirect = True, filename = None):
        print "\nInitializing MyApp..."
        wx.App.__init__(self, redirect, filename)
        
    def OnInit(self):
        self.parent_frame = MainWindow()
        self.parent_frame.Show()
        #self.SetTopWindow(self.parent_frame)
        #print >> sys.stderr, "A pretend error message"
        return True
    
    def OnExit(self):
        print "Exiting Myapp..."



        
if __name__ == '__main__':
    start_time = time.time()
    app = MyApp(redirect = False)
    print "Begin a MainLoop"
    app.MainLoop()    
    print "Finish a MainLoop"