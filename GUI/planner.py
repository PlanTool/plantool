#coding:utf-8
import re
import wx
from paras import PlannerWxParameters
import os
import sys
import time
import ctypes
import _ctypes
import logging
import argparse
import threading
import wx.html
import wx.lib.buttons as buttons
import planners.hsp
#from ctypes import *


#logging.basicConfig(filename='debug.log', filemode='w', level=logging.DEBUG)

def str2bool(v):
    return v.lower() in ("yes", "true", "t", "1")


class RunModelThread(threading.Thread):
    def __init__(self, name, func, args, window):
        threading.Thread.__init__(self)
        self.name = name
        self.func = func
        self.args = args
        self.window = window
        self.timeToQuit = threading.Event()
        self.timeToQuit.clear()

    def stop(self):
        self.timeToQuit.set()

    def run(self):
        msg = "Start running the thread %s\n"%self.name
        wx.CallAfter(self.window.thread_message, msg)

        self.func(self, self.args)
        wx.CallAfter(self.window.thread_finished, self)



class PlannerGUI(wx.Frame):
    def __init__(self, app_name, bcolor, track_id='D', app_size=(1000, 750)):
        #logging.debug( 'Initializing PlannerGUI...\n' )
        wx.Frame.__init__(self, None, -1, 'PlanTool', size=app_size)  
        self.panel = wx.Panel(self, -1)
        if bcolor:
            self.panel.SetBackgroundColour(bcolor)
        self.panel.Refresh()
        self.controls = {}
        self.lastAlgorithm = None
        self.currentPlanner = None
        self.track_id = track_id
        self.pddldir = './domains/Deterministic/'
        self.lib = ''
        self.dom_file = ''
        self.pro_file = ''
        self.count = 0
        self.thread_count = 0
        self.out_size = 11
        self.out_style = wx.DEFAULT
        
        self.set_app_name(app_name)
        self.show_planner()
        self.create_io_textbox()
        self.create_button()
        #self.create_toolbar()
        #self.initStatusBar() 
        self.createMenuBar()


    def thread_message(self, msg):
        self.OutputText.AppendText(msg)


    def thread_finished(self, thread):
        self.thread_count -= 1
        self.OutputText.AppendText('\n\nThread %s is finished.\n'%thread.name)
        self.OutputText.AppendText('\nCurrent threads amount: %d\n'%self.thread_count)   


    def set_app_name(self, app_name):
        self.app_name = wx.StaticText(self.panel, -1, app_name, (15, 15))
        self.app_name_font = wx.Font(32, wx.ROMAN, wx.ITALIC, wx.BOLD)
        self.app_name.SetFont(self.app_name_font)


    def show_planner(self):
        #Listbox
        self.planner_init()
        if self.track_id == 'D': #Deterministic Track
            self.PlannerList = ['IPP', 'HSP', 'FF', 'Graphplan','AltAlt','CSP','LAMA','Randward','SGplan','SatPlan2006','TLPlan']
            self.show_pddl_choice()
        elif self.track_id == 'U': #Uncertainty Track
            self.PlannerList = ['FPG','Glutton','MIPS','POMDP','Probabilistic-FF','T0'] 
        elif self.track_id == 'L': #Learning Track
            self.PlannerList = ['DUP','DPR','HTNML']
        self.planner_text, self.planner_choice = self.TF_choice("Select a planner:", 
            (15,150), (200, 145), self.PlannerList, self.choice_init)


    def planner_init(self):
        #print 'planner_init-----\n'
        self.planners = dict()
        self.planners["HSP"] = PlannerWxParameters("hsp",
                ["Algorithm","Direction","Heuristic"],
                ["-a","-d","-h"],
                ["choice","choice","choice"],
                [["bfs", "gbfs"],["forward", "backward"],["h1max", "h2plus", "h1plus"]],
                [["bfs", "gbfs"],["forward", "backward"],["h1max", "h2plus", "h1plus"]],
                                                   ["gbfs", "backward", "h1plus"],'-r')
#self.planners["IPP"] = PlannerWxParameters()
#       self.planners["FF"] = PlannerWxParameters()
#       self.planners["Graphplan"] = PlannerWxParameters()

        if self.track_id == 'D':
            self.hsp_a_text = 0
            self.hsp_d_text = 0 
            self.hsp_h_text = 0
            self.ff_info_text = 0
            self.ipp_info_text = 0
            self.gp_info_text = 0
            self.gp_default_text = 0

        elif self.track_id == 'L':
            self.train_DUP_flag = 0
            self.test_DUP_flag = 0
            self.DUP_wordvec_flag = 0
            self.test_DUP_text = 0
            self.train_DUP_text = 0
            #self.DPR_init()
            self.train_DPR_flag = 0
            self.test_DPR_flag = 0
            self.DPR_wordvec_flag = 0
            self.train_word2vec_text = 0
            self.ct_text = 0
            self.train_DPR_text = 0
            self.test_DPR_text = 0


    def show_pddl_choice(self):
        #Choose domain file and problem file
        cad = wx.StaticText(self.panel, -1, "Choose a domain:", (15,480), style=wx.ALIGN_LEFT)
        cap = wx.StaticText(self.panel, -1, "Choose a problem:", (15,520), style=wx.ALIGN_LEFT)
        cad.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.BOLD))
        cap.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.BOLD))
        self.dom_button = self.create_a_button("Choose", (150, 475), self.Domain_Button, 0, size=(180, -1))
        self.pro_button = self.create_a_button("Choose", (150, 515), self.Problem_Button, 0, size=(180, -1))


    def TF_choice(self, label, st_pos, ch_pos, choose_list, func=''):
        choose_text = wx.StaticText(self.panel, -1, label, st_pos, style=wx.ALIGN_LEFT)
        choose_text.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.BOLD))
        temp_choice = wx.Choice(self.panel, -1, ch_pos, choices=choose_list)
        temp_choice.SetSelection(-1)
        if func:
            self.Bind(wx.EVT_CHOICE, func, temp_choice)
        return choose_text, temp_choice


    def choice_init(self, event):
        p = self.planner_choice.GetSelection()
        if self.track_id == 'D' or self.track_id == 'U':
            currentAlgorithm = self.PlannerList[p]
            self.currentPlanner = self.planners[currentAlgorithm]
            if self.lastAlgorithm == "GraphPlan":
                self.__graphplan_destroy()
            for ctrl in self.controls.values():
                ctrl.Destroy()
            self.controls.clear()
            if currentAlgorithm == 'GraphPlan':
                self.__graphplan_show()
            self.__show(self.currentPlanner)
            self.lastAlgorithm = currentAlgorithm
        elif self.track_id == 'L':
            if p == 0:
                self.train_DPR_flag = 0
                self.test_DPR_flag = 0
                if self.train_word2vec_text:
                    self.train_word2vec_text.Destroy()
                    self.train_word2vec_choice.Destroy()
                if self.train_DPR_text:
                    self.train_DPR_destroy()
                if self.test_DPR_text:
                    self.test_DPR_destroy()
                if self.ct_text:
                    self.train_wordvec_destroy()
                self.DUP_init()
                self.train_DUP_list = ['False', 'True']
                self.train_DUP_text, self.train_DUP_choice = self.TF_choice("Train DUP model:",
                    (15,230), (200, 225), self.train_DUP_list, self.__train_DUP_show)

            elif p == 1:
                self.DPR_init()
                self.train_DUP_flag = 0
                self.test_DUP_flag = 0
                if self.train_DUP_text:
                    self.train_DUP_destroy()
                if self.train_DUP_flag:                  
                    if self.DUPSettingFrame:
                        self.DUPSettingFrame.Destroy()
                if self.test_DUP_text:                 
                    self.test_DUP_destroy()
                self.train_word2vec_list = ['False', 'True']
                self.train_word2vec_text, self.train_word2vec_choice = self.TF_choice("Train word2vec model:", 
                    (15,190), (200, 185), self.train_word2vec_list, self.__train_DPR_word2vec)


    def create_io_textbox(self):
        #Input textbox
        self.ofn, self.out_file_name = self.text_text_ctrl("Output file name:", (15,560), (150,555))

        #Output textbox
        output_label = wx.StaticText(self.panel, -1, "Realtime output", (440,20), (440,60), wx.ALIGN_CENTER)
        output_label_font = wx.Font(18, wx.SCRIPT, wx.NORMAL, wx.BOLD)
        output_label.SetFont(output_label_font)
        self.OutputText = wx.TextCtrl(self.panel, -1,"",
        size=(640, 560), pos=(340,60), style=wx.TE_MULTILINE | wx.TE_RICH2 )#| wx.TE_READONLY ) 
        output_font = wx.Font(self.out_size, self.out_style, wx.NORMAL, wx.NORMAL)
        self.OutputText.SetFont(output_font)
        self.pointer = self.OutputText.GetInsertionPoint()


    def create_button(self):
        self.create_a_button("Test", (100, 660), self.Test_Button, 0)
        self.create_a_button("Run", (200, 660), self.Run_Button, 0)
        self.create_a_button("Clear", (300, 660), self.Clear_Button, 0)
        self.create_a_button("Exit", (400, 660), self.Exit_Button, 0)


    def create_toolbar(self): pass


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


    def OnCloseWindow(self, event): 
        self.Destroy()


    def DPR_init(self):
        parser = argparse.ArgumentParser()
        envarg = parser.add_argument_group('Environment')
        envarg.add_argument("--model_dir", default="./models/word2vec_model/", help="The directory of word vector model.")
        envarg.add_argument("--vec_model", default='mymodel5-5-50', help="Word vector model name.")
        envarg.add_argument("--vec_length", type=int, default=50, help="Word vector dimension.")
        envarg.add_argument("--actionDB", default='tag_actions1', help="Tables' names in database test.")
        envarg.add_argument("--max_text_num", default='40', help="Max text num of database tables.")
        envarg.add_argument("--reward_assign", default='2.0 1.0 -1.0 -2.0', help="How the assign the rewards.")
        envarg.add_argument("--action_rate", type=float, default=0.15, help="Average actions percentage in a text.")
        envarg.add_argument("--penal_radix", type=float, default=5.0, help="Penalty radix according to action rate.")
        envarg.add_argument("--action_label", type=int, default=2, help="An integer refer to the label of actions.")
        envarg.add_argument("--non_action_label", type=int, default=1, help="An integer refer to the label of non-actions.")
        envarg.add_argument("--user", default='fengwf', help="Mysql account, user name.")
        envarg.add_argument("--passwd", default='123', help="Mysql password.")
        envarg.add_argument("--db", default='test', help="Mysql database name.")

        memarg = parser.add_argument_group('Replay memory')
        memarg.add_argument("--replay_size", type=int, default=10000, help="Maximum size of replay memory.")
        memarg.add_argument("--channel", type=int, default=1, help="Branches of CNN layers.")
        memarg.add_argument("--positive_rate", type=float, default=0.75, help="Choose how many positive examples per batch.")
        memarg.add_argument("--priority", default=1, help="Use the prioritized experience replay or not.")
        memarg.add_argument("--reward_bound", type=float, default=0, help="The boundary between positive examples and negative ones.")

        netarg = parser.add_argument_group('Deep Q-learning network')
        netarg.add_argument("--num_actions", type=int, default=1000, help="Total actions of this task.")
        netarg.add_argument("--words_num", type=int, default=500, help="Total words of an input text.")
        netarg.add_argument("--wordvec", type=int, default=100, help="Size of word vector.")
        netarg.add_argument("--learning_rate", type=float, default=0.0025, help="Learning rate.")
        netarg.add_argument("--momentum", type=float, default=0.1, help="Momentum of RMSPropOptimizer.")
        netarg.add_argument("--epsilon", type=float, default=1e-6, help="Epsilon of RMSPropOptimizer.")
        netarg.add_argument("--decay_rate", type=float, default=0.88, help="Decay rate for RMSPropOptimizer.")
        netarg.add_argument("--discount_rate", type=float, default=0.9, help="Discount rate for future rewards.")
        netarg.add_argument("--batch_size", type=int, default=8, help="Batch size for neural network.")
        netarg.add_argument("--target_output", type=int, default=2, help="Output dimension of DQN.")

        antarg = parser.add_argument_group('Agent')
        antarg.add_argument("--exploration_rate_start", type=float, default=1, help="Exploration rate at the beginning of decay.")
        antarg.add_argument("--exploration_rate_end", type=float, default=0.1, help="Exploration rate at the end of decay.")
        antarg.add_argument("--exploration_decay_steps", type=int, default=1000, help="How many steps to decay the exploration rate.")
        antarg.add_argument("--exploration_rate_test", type=float, default=0.0, help="Exploration rate used during testing.")
        antarg.add_argument("--train_frequency", type=int, default=1, help="Perform training after this many game steps.")
        antarg.add_argument("--train_repeat", type=int, default=1, help="Number of times to sample minibatch during training.")
        antarg.add_argument("--target_steps", type=int, default=5, help="Copy main network to target network after this many game steps.")
        antarg.add_argument("--random_play", default=0, help="Whether to perform random play.")

        mainarg = parser.add_argument_group('Main loop')
        mainarg.add_argument("--result_dir", default="./outputs/DPR_result.txt", help="The directory of output results.")
        mainarg.add_argument("--train_steps", type=int, default=18000, help="How many training steps per epoch.")
        mainarg.add_argument("--test_one", type=int, default=1, help="Test a text")
        mainarg.add_argument("--text_dir", default='./test_inputs/1.txt', help="A text for testing")
        mainarg.add_argument("--train_test", type=int, default=1, help="Test after training.")
        mainarg.add_argument("--test_text_num", type=int, default=4, help="How many texts for texting.")
        mainarg.add_argument("--epochs", type=int, default=2, help="How many epochs to run.")
        mainarg.add_argument("--start_epoch", type=int, default=0, help="Start from this epoch, affects exploration rate and names of saved snapshots.")
        mainarg.add_argument("--load_weights", default="./models/DPR_model/tb0_1.prm", help="Load network from file.")
        mainarg.add_argument("--save_weights_prefix", default="./models/DPR_model/tb0", help="Save network to given file. Epoch and extension will be appended.")
        mainarg.add_argument("--use_gpu", type=int, default=1, help="An integer, 0 for CPU only computer, else for GPU computer.")
        mainarg.add_argument("--gpu_rate", type=float, default=0.2, help="How much gpu memory to be use.")
        mainarg.add_argument("--cnn_format", default='NHWC', help="The format of tensorflow input matrixes")

        self.DPR_args = parser.parse_args()


    def __train_DPR_word2vec(self, event):
        d = self.train_word2vec_choice.GetSelection()
        if d != 0: #train word2vec model
            if self.train_DPR_text:
                self.train_DPR_destroy()
            self.DPR_wordvec_flag = 1
            self.ct_text = wx.StaticText(self.panel, -1, "Choose texts:", (15,340), style=wx.ALIGN_LEFT)
            self.ct_text.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.BOLD))
            self.ct_button = self.create_a_button("Choose", (150,340), self.DPR_word2vec_texts_Button, 0, size=(180, -1))
            self.cts, self.wvmodel_name = self.text_text_ctrl("Model_name:", (15, 230), (150, 225))
            self.mnc, self.mini_count = self.text_text_ctrl("Minimum count:", (15, 270), (150, 265))
            self.wvs, self.wordvec_size = self.text_text_ctrl("Wordvec size:", (15, 310), (150, 305))
        else:
            self.DPR_wordvec_flag = 0
            if self.ct_text:
                self.train_wordvec_destroy()
            self.train_DPR_list = ['False', 'True']
            self.train_DPR_text, self.train_DPR_choice = self.TF_choice("Train DPR model:",
                (15,230), (200, 225), self.train_DPR_list, self.__train_DPR_show)


    def train_wordvec(self, texts, model_name, mini_count, wordvec_size, workers=4):
        import gensim
        from gensim.models.word2vec import Word2Vec
        start = time.time()
        self.OutputText.AppendText('\nStart training word vectors...\n')
        model_dir = './models/word2vec_model/'
        sentences = gensim.models.word2vec.Text8Corpus(texts)
        model = gensim.models.word2vec.Word2Vec(sentences, min_count=mini_count, 
            size=wordvec_size, workers=workers)
        model.save_word2vec_format(model_dir+model_name, binary=False)
        end = time.time()
        self.OutputText.AppendText( "\nSuccessfully train a model and save as '%s'" %model_name )
        self.OutputText.AppendText( "\nTotal time cost: %ds" %(end-start) )


    def DPR_word2vec_texts_Button(self, event):
        text_type = "Text source (*.txt)|*.txt|" "All files (*.*)|*.*"
        dialog = wx.FileDialog(None, "Choose a file", './models/word2vec_model/', "", 
            text_type, wx.OPEN)
        if dialog.ShowModal() == wx.ID_OK:
            self.wv_texts = dialog.GetPath().encode('utf-8') 
            self.ct_button.SetLabelText(re.sub(r'.*/','',self.wv_texts))
        dialog.Destroy()


    def train_wordvec_destroy(self):
        self.ct_text.Destroy()
        self.ct_button.Destroy()
        self.cts.Destroy()
        self.wvmodel_name.Destroy()
        self.mnc.Destroy()
        self.mini_count.Destroy()
        self.wvs.Destroy()
        self.wordvec_size.Destroy()


    def DPR_main(self, thread, args):
        import tensorflow as tf
        from Environment import Environment
        from ReplayMemory import ReplayMemory
        from EADQN import DeepQLearner
        from Agent import Agent

        if args.words_num != 500:
            args.num_actions = 2*args.words_num
        tables_num = len(args.actionDB.split())
        #logging.debug( '\n\nargs of DPR:\n'+str(args)+'\n' ) 

        start = time.time()
        localtime = time.strftime("%Y-%m-%d %H:%M:%S",time.localtime(time.time()))
        #logging.debug('\nCurrent time is: %s'%localtime)
        #logging.debug('\nStarting at main.py...')
        self.OutputText.AppendText('\nCurrent time is: %s'%localtime)
        self.OutputText.AppendText('\n\nStart running DPR main function...\n')
        train_out = open(args.result_dir + "_train.txt",'w')
        test_out = open(args.result_dir + "_test.txt",'w')

        if args.use_gpu:
            args.cnn_format = 'NCHW'
            #Initial environment, replay memory, deep q net and agent
            gpu_options = tf.GPUOptions(per_process_gpu_memory_fraction=args.gpu_rate)
            with tf.Session(config=tf.ConfigProto(gpu_options=gpu_options)) as sess:
                net = DeepQLearner(args, sess)
                env = Environment(args)
                mem = ReplayMemory(args.replay_size, args)
                agent = Agent(env, mem, net, args)


                if args.load_weights:
                    #logging.debug('\nLoading weights from %s...\n'%args.load_weights)
                    self.OutputText.AppendText('\nLoading weights from %s...\n'%args.load_weights)
                    net.load_weights(args.load_weights)  

                if args.test_one:
                    ws, act_seq, st = agent.test_one(args.text_dir)
                    ##logging.debug('\nText_vec: %s'%str(env.text_vec))
                    #logging.debug('\nStates: %s\n'%str(st))
                    #logging.debug('\nWords: %s\n'%str(ws))
                    #logging.debug('\n\nAction_squence: %s\n'%str(act_seq))
                    #self.OutputText.AppendText('Words: %s\n'%str(ws))
                    for ind,s in enumerate(env.sents):
                        self.OutputText.AppendText('\nSentences %d: %s\n'%(ind,str(s)))
                    self.OutputText.AppendText('\nAction sequence:\n')
                    for ac in act_seq:
                        self.OutputText.AppendText('\t%s\n'%ac)
                else:
                    # loop over epochs
                    for epoch in xrange(args.start_epoch, args.epochs):
                        train_out.write(str(args)+'\n')
                        train_out.write('\nCurrent time is: %s'%localtime)
                        train_out.write('\nStarting at main.py...')
                        if args.train_steps:
                            agent.train(args.train_steps, epoch)
                            if args.save_weights_prefix:
                                filename = args.save_weights_prefix + "_%d.prm" % (epoch + 1)
                                net.save_weights(filename)

                        if args.train_steps > 0:
                            cnt = 0
                            ras = 0
                            tas = 0
                            tta = 0
                            for i in range(env.size):
                                text_vec_tags = env.saved_text_vec[i,:,-1]
                                state_tags = env.saved_states[i,:,-1]
                                sum_tags = sum(text_vec_tags)
                                if not sum_tags:
                                    break
                                count = 0
                                right_actions = 0
                                tag_actions = 0
                                total_actions = 0
                                total_words = args.num_actions/2
                                temp_words = env.saved_text_length[i]
                                if temp_words > total_words:
                                    temp_words = total_words

                                #logging.debug( "text_vec_tags",text_vec_tags )
                                #logging.debug( 'state_tags',state_tags )
                                for t in text_vec_tags:
                                    if t == args.action_label:
                                        total_actions += 1

                                train_out.write('\n\nText:'+str(i))
                                train_out.write('\ntotal words: %d\n'%temp_words)
                                #logging.debug( '\ntotal words: %d\n'%temp_words )
                                train_out.write('\nsaved_text_vec:\n')
                                train_out.write(str(env.saved_text_vec[i,:,-1]))
                                train_out.write('\nsaved_states:\n')
                                train_out.write(str(env.saved_states[i,:,-1]))

                                for s in xrange(temp_words):
                                    if state_tags[s] == 0:
                                        count += 1
                                    elif state_tags[s] == args.action_label:
                                        tag_actions += 1
                                        if text_vec_tags[s] == state_tags[s]:
                                            right_actions += 1

                                cnt += count
                                ras += right_actions
                                tta += tag_actions
                                tas += total_actions
                                if total_actions > 0:
                                    recall = float(right_actions)/total_actions
                                else:
                                    recall = 0
                                if tag_actions > 0:
                                    precision = float(right_actions)/tag_actions
                                else:
                                    precision = 0
                                rp = recall + precision
                                if rp > 0:
                                    F_value = (2.0*recall*precision)/(recall+precision)
                                else:
                                    F_value = 0
                                train_out.write('\nWords left: %d'%count)
                                train_out.write('\nAcions: %d'%total_actions)
                                train_out.write('\nRight_actions: %d'%right_actions)
                                train_out.write('\nTag_actions: %d'%tag_actions)
                                train_out.write('\nActions_recall: %f'%recall)
                                train_out.write('\nActions_precision: %f'%precision)
                                train_out.write('\nF_measure: %f'%F_value)
                                #logging.debug( '\nText: %d'%i )
                                #logging.debug( '\nWords left: %d'%count )
                                #logging.debug( 'Acions: %d'%total_actions )
                                #logging.debug( 'Right_actions: %d'%right_actions )
                                #logging.debug( 'Tag_actions: %d'%tag_actions )
                                #logging.debug( 'Actions_recall: %f'%recall )
                                #logging.debug( 'Actions_precision: %f'%precision )
                                #logging.debug( 'F_measure: %f'%F_value )

                            if tas > 0:
                                average_recall = float(ras)/tas
                            else:
                                average_recall = 0
                            if tta > 0:
                                average_precision = float(ras)/tta
                            else:
                                average_precision = 0
                            arp = average_recall + average_precision
                            if arp > 0:
                                ave_F_value = (2*average_recall*average_precision)/(average_recall+average_precision)
                            else:
                                ave_F_value = 0
                            train_out.write('\nTotal words left: %d'%cnt)
                            train_out.write('\nTotal acions: %d'%tas)
                            train_out.write('\nTotal right_acions: %d'%ras)
                            train_out.write('\nTotal tag_acions: %d'%tta)
                            train_out.write('\nAverage_actions_recall: %f'%average_recall)
                            train_out.write('\nAverage_actions_precision: %f'%average_precision)
                            train_out.write('\nAverage_F_measure: %f'%ave_F_value)
                            #logging.debug( '\nTotal words left: %d'%cnt )
                            #logging.debug( 'Total acions: %d'%tas )
                            #logging.debug( 'Total right_actions: %d'%ras )
                            #logging.debug( 'Total tag_actions: %d'%tta )
                            #logging.debug( 'Average_actions_recall: %f'%average_recall )
                            #logging.debug( 'Average_actions_precision: %f'%average_precision )
                            #logging.debug( 'Average_F_measure: %f'%ave_F_value )


                        if args.train_test:
                            agent.train_test(args.words_num, args.test_text_num*tables_num, test_out)
                        train_out.close()
                        test_out.close()

                end = time.time()
                localtime = time.strftime("%Y-%m-%d %H:%M:%S",time.localtime(time.time()))
                #logging.debug('\nTotal time cost: %ds'%(end-start))
                #logging.debug('\nCurrent time is: %s\n'%localtime)
                self.OutputText.AppendText('Total time cost: %ds\n'%(end-start))
                self.OutputText.AppendText('Current time is: %s\n'%localtime)
        else:
            with tf.Session() as sess:
                net = DeepQLearner(args, sess)
                env = Environment(args)
                mem = ReplayMemory(args.replay_size, args)
                agent = Agent(env, mem, net, args)


                if args.load_weights:
                    #logging.debug('\nLoading weights from %s...\n'%args.load_weights)
                    self.OutputText.AppendText('\nLoading weights from %s...\n'%args.load_weights)
                    net.load_weights(args.load_weights)  

                if args.test_one:
                    ws, act_seq, st = agent.test_one(args.text_dir)
                    ##logging.debug('\nText_vec: %s'%str(env.text_vec))
                    #logging.debug('\nStates: %s\n'%str(st))
                    #logging.debug('\nWords: %s\n'%str(ws))
                    #logging.debug('\n\nAction_squence: %s\n'%str(act_seq))
                    self.OutputText.AppendText('Words: %s\n'%str(ws))
                    self.OutputText.AppendText('\nAction_squence: %s\n'%str(act_seq))
                else:
                    # loop over epochs
                    for epoch in xrange(args.start_epoch, args.epochs):
                        train_out.write(str(args)+'\n')
                        train_out.write('\nCurrent time is: %s'%localtime)
                        train_out.write('\nStarting at main.py...')
                        if args.train_steps:
                            agent.train(args.train_steps, epoch)
                            if args.save_weights_prefix:
                                filename = args.save_weights_prefix + "_%d.prm" % (epoch + 1)
                                net.save_weights(filename)

                        if args.train_steps > 0:
                            cnt = 0
                            ras = 0
                            tas = 0
                            tta = 0
                            for i in range(env.size):
                                text_vec_tags = env.saved_text_vec[i,:,-1]
                                state_tags = env.saved_states[i,:,-1]
                                sum_tags = sum(text_vec_tags)
                                if not sum_tags:
                                    break
                                count = 0
                                right_actions = 0
                                tag_actions = 0
                                total_actions = 0
                                total_words = args.num_actions/2
                                temp_words = env.saved_text_length[i]
                                if temp_words > total_words:
                                    temp_words = total_words

                                #logging.debug( "text_vec_tags",text_vec_tags )
                                #logging.debug( 'state_tags',state_tags )
                                for t in text_vec_tags:
                                    if t == args.action_label:
                                        total_actions += 1

                                train_out.write('\n\nText:'+str(i))
                                train_out.write('\ntotal words: %d\n'%temp_words)
                                #logging.debug( '\ntotal words: %d\n'%temp_words )
                                train_out.write('\nsaved_text_vec:\n')
                                train_out.write(str(env.saved_text_vec[i,:,-1]))
                                train_out.write('\nsaved_states:\n')
                                train_out.write(str(env.saved_states[i,:,-1]))

                                for s in xrange(temp_words):
                                    if state_tags[s] == 0:
                                        count += 1
                                    elif state_tags[s] == args.action_label:
                                        tag_actions += 1
                                        if text_vec_tags[s] == state_tags[s]:
                                            right_actions += 1

                                cnt += count
                                ras += right_actions
                                tta += tag_actions
                                tas += total_actions
                                if total_actions > 0:
                                    recall = float(right_actions)/total_actions
                                else:
                                    recall = 0
                                if tag_actions > 0:
                                    precision = float(right_actions)/tag_actions
                                else:
                                    precision = 0
                                rp = recall + precision
                                if rp > 0:
                                    F_value = (2.0*recall*precision)/(recall+precision)
                                else:
                                    F_value = 0
                                train_out.write('\nWords left: %d'%count)
                                train_out.write('\nAcions: %d'%total_actions)
                                train_out.write('\nRight_actions: %d'%right_actions)
                                train_out.write('\nTag_actions: %d'%tag_actions)
                                train_out.write('\nActions_recall: %f'%recall)
                                train_out.write('\nActions_precision: %f'%precision)
                                train_out.write('\nF_measure: %f'%F_value)
                                #logging.debug( '\nText: %d'%i )
                                #logging.debug( '\nWords left: %d'%count )
                                #logging.debug( 'Acions: %d'%total_actions )
                                #logging.debug( 'Right_actions: %d'%right_actions )
                                #logging.debug( 'Tag_actions: %d'%tag_actions )
                                #logging.debug( 'Actions_recall: %f'%recall )
                                #logging.debug( 'Actions_precision: %f'%precision )
                                #logging.debug( 'F_measure: %f'%F_value )

                            if tas > 0:
                                average_recall = float(ras)/tas
                            else:
                                average_recall = 0
                            if tta > 0:
                                average_precision = float(ras)/tta
                            else:
                                average_precision = 0
                            arp = average_recall + average_precision
                            if arp > 0:
                                ave_F_value = (2*average_recall*average_precision)/(average_recall+average_precision)
                            else:
                                ave_F_value = 0
                            train_out.write('\nTotal words left: %d'%cnt)
                            train_out.write('\nTotal acions: %d'%tas)
                            train_out.write('\nTotal right_acions: %d'%ras)
                            train_out.write('\nTotal tag_acions: %d'%tta)
                            train_out.write('\nAverage_actions_recall: %f'%average_recall)
                            train_out.write('\nAverage_actions_precision: %f'%average_precision)
                            train_out.write('\nAverage_F_measure: %f'%ave_F_value)
                            #logging.debug( '\nTotal words left: %d'%cnt )
                            #logging.debug( 'Total acions: %d'%tas )
                            #logging.debug( 'Total right_actions: %d'%ras )
                            #logging.debug( 'Total tag_actions: %d'%tta )
                            #logging.debug( 'Average_actions_recall: %f'%average_recall )
                            #logging.debug( 'Average_actions_precision: %f'%average_precision )
                            #logging.debug( 'Average_F_measure: %f'%ave_F_value )


                        if args.train_test:
                            agent.train_test(args.words_num, args.test_text_num*tables_num, test_out)
                        train_out.close()
                        test_out.close()

                end = time.time()
                localtime = time.strftime("%Y-%m-%d %H:%M:%S",time.localtime(time.time()))
                #logging.debug('\nTotal time cost: %ds'%(end-start))
                #logging.debug('\nCurrent time is: %s\n'%localtime)
                self.OutputText.AppendText('Total time cost: %ds\n'%(end-start))
                self.OutputText.AppendText('Current time is: %s\n'%localtime)

        tf.reset_default_graph()


    def __train_DPR_show(self, event):
        c = self.train_DPR_choice.GetSelection()
        if c != 0:  # train DPR model 
            if self.test_DPR_text:
                self.test_DPR_flag = 0
                self.test_DPR_destroy()
            self.DPRSettingFrame = DPR_Settings(self.DPR_args)
            self.DPRSettingFrame.Show()
            self.train_DPR_flag = 1
        else:  # test only
            self.train_DPR_flag = 0
            self.test_DPR_text = wx.StaticText(self.panel, -1, "Choose a text", 
                (15,340), style=wx.ALIGN_LEFT)
            self.test_DPR_text.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.BOLD))
            self.test_DPR_button = self.create_a_button("Choose", (150,335), 
                self.test_DPR_Button, 0, size=(180, -1))
         

    def test_DPR_Button(self, event):
        text_type = "Text source (*.txt)|*.txt|" "All files (*.*)|*.*"
        dialog = wx.FileDialog(None, "Choose a file", './test_inputs/', "", 
            text_type, wx.OPEN)
        if dialog.ShowModal() == wx.ID_OK:
            self.text_dir = dialog.GetPath().encode('utf-8') 
            self.test_DPR_button.SetLabelText(re.sub(r'.*/','',self.text_dir))
            self.test_DPR_flag = 1
        dialog.Destroy()


    def test_DPR_destroy(self):
        self.test_DPR_text.Destroy()
        self.test_DPR_button.Destroy()


    def train_DPR_destroy(self):
        self.train_DPR_text.Destroy()
        self.train_DPR_choice.Destroy()


    def DUP_init(self):
        parser = argparse.ArgumentParser()
        parser.add_argument("--domain_dir", default="./domains/DUP/blocks/", help="")
        parser.add_argument("--domain", default="blocks5000.txt", help="")
        parser.add_argument("--k", type=int, default=10, help="")
        parser.add_argument("--split_texts", type=int, default=0, help="")
        parser.add_argument("--mask", default="###", help="")
        parser.add_argument("--test_only", type=int, default=0, help="")
        parser.add_argument("--test_text", default="./domains/DUP/blocks/test.txt", help="")
        parser.add_argument("--train_word2vec", type=int, default=1, help="")    
        parser.add_argument("--workers", type=int, default=4, help="")
        parser.add_argument("--min_count", type=int, default=1, help="")
        parser.add_argument("--word2vec_iter", type=int, default=20, help="")
        parser.add_argument("--iter_num", type=int, default=1, help="")
        parser.add_argument("--learning_rate", type=float, default=0.01, help="")
        parser.add_argument("--window_size", type=int, default=1, help="")
        parser.add_argument("--blank_percentage", type=float, default=0.05, help="")
        parser.add_argument("--prediction_set_size", type=int, default=10, help="")
        self.DUP_args = parser.parse_args()


    def DUP_main(self, thread, args):
        from shallow_plan import ShallowPlan
        SPlan = ShallowPlan(args, self.OutputText)
        SPlan.run()


    def DUP_test(self, thread, args):
        from shallow_plan import ShallowPlan
        SPlan = ShallowPlan(args, self.OutputText)
        SPlan.test('')


    def __train_DUP_show(self, event):
        c = self.train_DUP_choice.GetSelection()
        if c != 0:
            if self.test_DUP_text:
                self.test_DUP_flag = 0
                self.test_DUP_destroy()
            self.DUPSettingFrame = DUP_Settings(self.DUP_args)
            self.DUPSettingFrame.Show()
            self.train_DUP_flag = 1
        else:
            if self.train_DUP_flag:
                self.train_DUP_flag = 0
                if self.DUPSettingFrame:
                    self.DUPSettingFrame.Destroy()
            self.test_DUP_text = wx.StaticText(self.panel, -1, "Choose a text", 
                (15,340), style=wx.ALIGN_LEFT)
            self.test_DUP_text.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.BOLD))
            self.test_DUP_button = self.create_a_button("Choose", (150,335), 
                self.test_DUP_Button, 0, size=(180, -1))


    def train_DUP_destroy(self):
        self.train_DUP_text.Destroy()
        self.train_DUP_choice.Destroy()


    def test_DUP_destroy(self):
        self.test_DUP_text.Destroy()
        self.test_DUP_button.Destroy()


    def test_DUP_Button(self, event):
        text_type = "Text source (*.txt)|*.txt|" "All files (*.*)|*.*"
        dialog = wx.FileDialog(None, "Choose a file", './domains/DUP/blocks/', "", 
            text_type, wx.OPEN)
        if dialog.ShowModal() == wx.ID_OK:
            self.DUP_test_input = dialog.GetPath().encode('utf-8') 
            self.test_DUP_button.SetLabelText(re.sub(r'.*/','',self.DUP_test_input))
            self.test_DUP_flag = 1
        dialog.Destroy()


    def create_a_button(self, label, pos, func, flag, size=(80,-1)):
        button = wx.Button(self.panel, -1, label, size=size, pos=pos)  
        self.Bind(wx.EVT_BUTTON, func, button)
        if flag:
            self.button.SetDefault()
        return button


    def text_text_ctrl(self, label, st_pos, ct_pos, font_size=10, size=(180, -1)):
        text = wx.StaticText(self.panel, -1, label, st_pos, style=wx.ALIGN_LEFT)
        text.SetFont(wx.Font(font_size, wx.DEFAULT, wx.NORMAL, wx.BOLD))
        textctrl = wx.TextCtrl(self.panel, -1, "", size=size, pos=ct_pos, style=wx.TE_PROCESS_ENTER)
        textctrl.SetInsertionPoint(0)
        return text, textctrl

    def ipp_help(self):
        self.pointer = self.OutputText.GetInsertionPoint()
        self.OutputText.AppendText("\n\nusage of ipp:\n\n");
        self.OutputText.AppendText("run-time information level( preset: 1 )\n");
        self.OutputText.AppendText("     1      info on action number, graph, search and plan\n");
        self.OutputText.AppendText("     2      1 + info on problem constants, types and predicates\n");  
        self.OutputText.AppendText("     3      1 + 2 + loaded operators, initial and goal state\n");
        self.OutputText.AppendText("     4      1 + predicates and their inertia status\n");
        self.OutputText.AppendText("     5      1 + 4 + goal state and operators with unary inertia encoded\n");
        self.OutputText.AppendText("     6      1 + actions, initial and goal state after expansion of variables\n"); 
        self.OutputText.AppendText("     7      1 + facts selected as relevant to the problem\n");
        self.OutputText.AppendText("     8      1 + final domain representation\n");
        tp_point = self.pointer
        self.pointer = self.OutputText.GetInsertionPoint()
        self.OutputText.SetStyle(tp_point, self.pointer, wx.TextAttr("sienna", wx.NullColour))

    def __show(self, planner):
        y = 190
        for name, typ, label, default in zip(planner.names, \
            planner.types, planner.labels, planner.defaults):
            if typ == "choice":
                self.controls[name+"T"],self.controls[name] = self.TF_choice(name+":",
                            (15, y), (200, y-5), label)
                if default is not None and default in label:
                    self.controls[name].SetSelection(label.index(default))
                y += 40
            elif typ == "text":
                self.controls[name+"T"],self.controls[name] = \
                    self.text_text_ctrl(name+":",
                            (15, y), (200, y-5),
                            size = (120, -1))
                if default is not None:
                    self.controls[name].setStringValue(default)
                y += 40
            elif typ == "checkbox":
                self.controls[name+"T"], self.controls[name] = \
                        self.checkBox(y, label)
                if default is not None:
                    self.controls[name].SetChecked(default)
                y += 130
        eval("self."+self.currentPlanner.algName+"_help()")
        
    def checkBox(self, y, options):
        gp_options_text = wx.StaticText(self.panel, -1, 
            "Options:", (15,y), style=wx.ALIGN_LEFT)
        gp_options_text.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.BOLD))
        gp_options_choices = wx.CheckListBox(self.panel, -1, (150,y-10),
                (180,120),options, wx.LB_MULTIPLE)
        return gp_options_text, gp_options_choices


    def hsp_help(self):
        self.pointer = self.OutputText.GetInsertionPoint()
        self.OutputText.AppendText("\n\nusage of hsp:\n\n");
        self.OutputText.AppendText("OPTIONS   DESCRIPTIONS\n\n");
        self.OutputText.AppendText("Algorithm:\n\tEither 'bfs' or 'gbfs'.\n" );
        self.OutputText.AppendText("Direction of Searching:\n\tEither 'forward' or 'backward'.\n" );
        self.OutputText.AppendText("Heuristic Methods:\n\tOne of 'h1plus', 'h1max', 'h2plus', 'h2max'.\n\n" );
        tp_point = self.pointer
        self.pointer = self.OutputText.GetInsertionPoint()
        self.OutputText.SetStyle(tp_point, self.pointer, wx.TextAttr("coral", wx.NullColour))

    def ff_help(self):
        self.pointer = self.OutputText.GetInsertionPoint()
        self.OutputText.AppendText("\n\nusage of ff:\n");
        self.OutputText.AppendText("run-time information level( preset: 1 )\n");
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

    def __graphplan_show(self):
        self.gp_default_list = ['False', 'True']
        self.gp_default_text, self.gp_default_choice = self.TF_choice("Default setting:", 
            (15,190), (200, 185), self.gp_default_list, self.__graphplan_setting)
        self.__graphplan_help()


    def __graphplan_help(self):
        self.pointer = self.OutputText.GetInsertionPoint()
        self.OutputText.AppendText("\n\nusage of graphplan:\n");
        self.OutputText.AppendText("\nOPTIONS   DESCRIPTIONS\n\n");
        self.OutputText.AppendText("Default Setting:\n\tuse default values or not.\n");
        self.OutputText.AppendText("Time Steps:\n\tto specify a fixed number of time steps\n");
        self.OutputText.AppendText("Information Level\n\tto specify info level 1 or 2 (default is 0)\n\n");
        self.OutputText.AppendText("Options\n\tto specify options you want\n");
        tp_point = self.pointer
        self.pointer = self.OutputText.GetInsertionPoint()
        self.OutputText.SetStyle(tp_point, self.pointer, wx.TextAttr("green", wx.NullColour))


    def __graphplan_setting(self, event):
        d = self.gp_default_choice.GetSelection()
        if d == 0:
            self.gp_info_list = [str(i) for i in range(3)]
            self.gp_info_text, self.gp_info_choice = self.TF_choice("Information level:", 
            (15,230), (200, 225), self.gp_info_list)

            self.gp_time_steps_text, self.gp_time_steps = self.text_text_ctrl("Time steps:", 
                (15,270), (200,265), size=(120, -1))

            self.gp_options_text = wx.StaticText(self.panel, -1, 
                "Options:", (15,310), style=wx.ALIGN_LEFT)
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


    def test_thread(self, thread, args):
        for i in range(1, 10):
            thread.timeToQuit.wait(args)
            if thread.timeToQuit.isSet():
                break
            msg = "Message %d from thread %s\n"%(i, thread.name)
            self.thread_message(msg)
        self.thread_finished(thread)


    def Test_Button(self, event): 
        pass
        '''
        self.test_DUP_flag = 1 - self.test_DUP_flag
        self.OutputText.AppendText('\n-----  self.train_DUP_flag: %d  -----\n'%self.train_DUP_flag)
        self.OutputText.AppendText('\n-----  self.test_DUP_flag: %d  -----\n'%self.test_DUP_flag)
        self.OutputText.AppendText('\n-----  self.train_DPR_flag: %d  -----\n'%self.train_DPR_flag)
        self.OutputText.AppendText('\n-----  self.test_DPR_flag: %d  -----\n'%self.test_DPR_flag)
        self.OutputText.AppendText('\nStart testing DPR model...\n')

        if self.thread_count == 0 and self.test_DPR_flag:
            dic = self.DPR_args.__dict__
            for k in dic.keys():
                self.OutputText.AppendText('%s:\t%s\n'%(k, str(dic[k])))
            self.DPRthreadTest = RunModelThread('DPRthreadTest', self.DPR_main, self.DPR_args, self)
            self.DPRthreadTest.start()
            self.thread_count += 1
        
        if self.count == 0:
            self.thread = RunModelThread('test_thread', self.test_thread, 1.5, self)
            self.thread.start()
        elif self.count == 1:
            self.DPRthreadTest = RunModelThread('DPRthreadTest', self.DPR_main, self.DPR_args, self)
            self.DPRthreadTest.start()
        else:
            self.thread.stop()
        self.count += 1
        
        if self.DPR_args:
            print '\nself.DPR_args.vec_length',self.DPR_args.vec_length
            if self.DPR_args.vec_length == 50:
                print '\n-----args not change!'
            argsDict = self.DPR_args.__dict__
            for i in argsDict.keys():
                self.OutputText.AppendText('%s:    %s\n'%(str(i), str(argsDict[i])))
        
        self.lib = ctypes.CDLL('./libs/ff.so')
        self.dom_file = './pddl_files/ff_domain.pddl'
        self.pro_file = './pddl_files/ff_problem.pddl'
        result_name = 'test_ff_result.txt'
        argv = ['ff', '-o', self.dom_file, '-f', self.pro_file, '-r', result_name]
        self.OutputText.AppendText('\nInput argv is %s\n'%' '.join(argv))
        self.OutputText.AppendText('\nDoing planning for your problem...')
        self.lib.main(len(argv), (ctypes.c_char_p*len(argv))(*argv))
        with open(result_name) as f:
            self.OutputText.AppendText(f.read())
        _ctypes.dlclose(self.lib._handle)
        self.lib = ''
        self.OutputText.AppendText('\nThe planning is done!\n')
        #logging.debug( 'testing...\n' )
        self.count += 1
        self.statusBar.SetStatusText('tesing %d'%self.count, self.count%3)
        '''


    def Run_Button(self, event):
        ind = self.planner_choice.GetSelection()
        if ind == -1:
            self.OutputText.AppendText("\nError: no planner selected!\n")
            return

        tool_name = self.PlannerList[ind]
        self.OutputText.AppendText("\nUsing planner %s...\n"%tool_name)

        pl = self.PlannerList[ind].lower()
        temp = self.out_file_name.GetValue().encode('utf-8').split()
        self.out_file_name.Clear()

        if self.track_id == 'D':
            if not self.dom_file:
                self.OutputText.AppendText("\nError: no domain file selected!\n")
                return
            elif not self.pro_file:
                self.OutputText.AppendText("\nError: no problem file selected!\n")
                return
            argv = []
            if pl == 'graphplan':       
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
            elif self.currentPlanner:
                argv.append(self.currentPlanner.algName)
                argv.extend(['-o', self.dom_file, '-f', self.pro_file,])
                if temp:
                    argv.extend([self.currentPlanner.outtag, temp[0]])
                else:
                    argv.extend([self.currentPlanner.outtag, pl+"_output.txt"])
                for i in range(len(self.currentPlanner.names)):
                    if self.currentPlanner.types[i] == "text":
                        text = self.controls[self.currentPlanner.names[i]].GetValue().encode('utf-8').strip()
                        if not text:
                            self.OutputText.AppendText('\nError : Empty '+self.currentPlanner.names[i])
                            return
                        argv.extend([self.currentPlanner.tags[i],
                                text])
                    elif self.currentPlanner.types[i] == "choice":
                        idx = self.controls[self.currentPlanner.names[i]].GetSelection()
                        argv.extend([self.currentPlanner.tags[i],
                                self.currentPlanner.values[i][idx]])
                    elif self.currentPlanner.types[i] == "checkbox":
                        text = ""
                        for j in range(len(self.currentPlanner.values[i])):
                            if self.controls[self.currentPlanner.names[i]].IsChecked(j):
                                text += self.currentPlanner.values[i][j]
                        argv.extend([self.currentPlanner.tags[i],
                                text])
            else:
                return

            self.OutputText.AppendText('\nInput argv is %s\n'%' '.join(argv))
            self.OutputText.AppendText('\nDoing planning for your problem...')
            eval("planners."+self.currentPlanner.algName+".run(argv)")
            if temp:
                self.OutputText.AppendText(
                    '\nThe planning is done!\nResult of it is saved in %s\n'%temp[0])
            else:
                #self.OutputText.AppendText( pl+'_output.txt' )
                #with open(pl+'_output.txt') as f:
                if os.path.isfile(pl+'_output.txt'):
                    f = open(pl+'_output.txt')
                    self.OutputText.AppendText(f.read())
                    self.OutputText.AppendText('\nThe planning is done!\n')
                    f.close()
                    os.system('rm '+pl+'_output.txt')

        elif self.track_id == 'U':
            pass

        elif self.track_id == 'L':
            if self.DPR_wordvec_flag:
                name = self.wvmodel_name.GetValue().encode('utf-8').split()
                count = self.mini_count.GetValue().encode('utf-8').split()
                size = self.wordvec_size.GetValue().encode('utf-8').split()
                if not name:
                    self.OutputText.AppendText('\nError: no model name preset!\n')
                    return
                elif not count:
                    self.OutputText.AppendText('\nError: no minimum count preset!\n')
                    return
                elif not size:
                    self.OutputText.AppendText('\nError: no word vector size preset!\n')
                    return
                else:
                    model_name = name[0]
                    mini_count = int(count[0])
                    wordvec_size = int(size[0])
                self.train_wordvec(self.wv_texts, model_name, mini_count, wordvec_size)

            elif self.test_DPR_flag:
                self.OutputText.AppendText('\nStart testing DPR model...\n')
                self.OutputText.AppendText('\nAll arguments of DPr model:\n')
                dic = self.DPR_args.__dict__
                for k in dic.keys():
                    self.OutputText.AppendText('\t%s:\t\t%s\n'%(k, str(dic[k])))
                #self.OutputText.AppendText('\nIt will take dozens of seconds. Please wait patiently.\n')
                #tp_point = self.pointer
                #self.pointer = self.OutputText.GetInsertionPoint()
                #self.OutputText.SetStyle(tp_point, self.pointer, wx.TextAttr("red", wx.NullColour))
                self.DPR_main('', self.DPR_args)
                #self.DPRthreadTest = RunModelThread('DPRthreadTest', self.DPR_main, self.DPR_args, self)
                #self.DPRthreadTest.start()
                #self.thread_count += 1

            elif self.train_DPR_flag:
                self.OutputText.AppendText('\nStart training DPR model...\n')
                dic = self.DPR_args.__dict__
                for k in dic.keys():
                    self.OutputText.AppendText('%s:\t%s\n'%(k, str(dic[k])))
                self.DPRthreadTrain = RunModelThread('DPRthreadTrain', self.DPR_main, self.DPR_args, self)
                self.DPRthreadTrain.start()
                self.thread_count += 1

            elif self.train_DUP_flag:
                self.OutputText.AppendText('\nStart training DUP model...\n')
                dic = self.DUP_args.__dict__
                for k in dic.keys():
                    self.OutputText.AppendText('%s:\t%s\n'%(k, str(dic[k])))
                #from shallow_plan import ShallowPlan
                #SPlan = ShallowPlan(self.DUP_args, self.OutputText)
                #SPlan.run()
                self.DUPthreadTrain = RunModelThread('DUPthreadTrain', self.DUP_main, self.DUP_args, self)
                self.DUPthreadTrain.start()
                self.thread_count += 1

            elif self.test_DUP_flag:
                self.OutputText.AppendText('\nStart testing DUP model...\n')
                self.OutputText.AppendText('\nAll arguments of DUP model:\n')
                dic = self.DUP_args.__dict__
                for k in dic.keys():
                    self.OutputText.AppendText('\t%s:\t\t%s\n'%(k, str(dic[k])))
                from shallow_plan import ShallowPlan
                SPlan = ShallowPlan(self.DUP_args, self.OutputText)
                SPlan.test('')
                #self.DUPthreadTest = RunModelThread('DUPthreadTest', self.DUP_test, self.DUP_args, self)
                #self.DUPthreadTest.start()
                #self.thread_count += 1
            else:
                pass
                 

    def Clear_Button(self, event): 
        self.OutputText.Clear()


    def Exit_Button(self, event): 
        if self.lib:
            _ctypes.dlclose(self.lib._handle)
        self.Destroy()




class DPR_Settings(wx.Frame):
    def __init__(self, args):
        wx.Frame.__init__(self, None, -1, "DPR Model Settings")
        self.panel = wx.Panel(self)
        self.panel.SetBackgroundColour('khaki')
        self.args = args
        self.weights_dir = ''

        self.show_init()
        sub_box1 = wx.BoxSizer(wx.VERTICAL)
        sub_box2 = wx.BoxSizer(wx.VERTICAL)
        box = wx.BoxSizer(wx.HORIZONTAL)
        sub_box1.Add(self.sub_sub_box(self.vec_len_t, self.vec_len_tc))
        sub_box1.Add(self.sub_sub_box(self.actionDB_t, self.actionDB_tc))
        sub_box1.Add(self.sub_sub_box(self.replay_size_t, self.replay_size_tc))
        sub_box1.Add(self.sub_sub_box(self.words_num_t, self.words_num_tc))
        sub_box1.Add(self.sub_sub_box(self.batch_size_t, self.batch_size_tc))
        sub_box1.Add(self.sub_sub_box(self.load_w_t, self.load_w_button))
        sub_box2.Add(self.sub_sub_box(self.epochs_t, self.epochs_tc))
        sub_box2.Add(self.sub_sub_box(self.w_prefix_t, self.w_prefix_tc))
        sub_box2.Add(self.sub_sub_box(self.gpu_rate_t, self.gpu_rate_tc))
        sub_box2.Add(self.sub_sub_box(self.test_num_t, self.test_num_tc))
        sub_box2.Add(self.sub_sub_box(self.texts_amount_t, self.texts_amount_tc))
        sub_box2.Add(self.sub_sub_box(self.set_def_but, self.save_button))
        box.Add(sub_box1, 0, wx.ALL, 10)
        box.Add(sub_box2, 0, wx.ALL, 10)

        self.panel.SetSizer(box)
        box.Fit(self)


    def sub_sub_box(self, text, textctrl, style=wx.ALL|wx.EXPAND, gaps=5):
        box = wx.BoxSizer(wx.HORIZONTAL)
        box.Add(text, 0, style, gaps)
        box.Add(textctrl, 0, style, gaps)
        return box


    def show_init(self):
        size = (120, -1)
        self.vec_len_t = wx.StaticText(self.panel, -1, "Wordvec length:", size=size)
        self.vec_len_tc = wx.TextCtrl(self.panel, -1, "", size=size, style=wx.TE_PROCESS_ENTER)
        self.vec_len_tc.SetInsertionPoint(0)

        self.actionDB_t = wx.StaticText(self.panel, -1, "Action database:", size=size)
        self.actionDB_tc = wx.TextCtrl(self.panel, -1, "", size=size, style=wx.TE_PROCESS_ENTER)
        self.actionDB_tc.SetInsertionPoint(0)

        self.replay_size_t = wx.StaticText(self.panel, -1, "Replay size:", size=size)
        self.replay_size_tc = wx.TextCtrl(self.panel, -1, "", size=size, style=wx.TE_PROCESS_ENTER)
        self.replay_size_tc.SetInsertionPoint(0)

        self.words_num_t = wx.StaticText(self.panel, -1, "Text length:", size=size)
        self.words_num_tc = wx.TextCtrl(self.panel, -1, "", size=size, style=wx.TE_PROCESS_ENTER)
        self.words_num_tc.SetInsertionPoint(0)

        self.batch_size_t = wx.StaticText(self.panel, -1, "Batch size:", size=size)
        self.batch_size_tc = wx.TextCtrl(self.panel, -1, "", size=size, style=wx.TE_PROCESS_ENTER)
        self.batch_size_tc.SetInsertionPoint(0)

        self.epochs_t = wx.StaticText(self.panel, -1, "Training epochs:", size=size)
        self.epochs_tc = wx.TextCtrl(self.panel, -1, "", size=size, style=wx.TE_PROCESS_ENTER)
        self.epochs_tc.SetInsertionPoint(0)

        self.w_prefix_t = wx.StaticText(self.panel, -1, "Weight prefix:", size=size)
        self.w_prefix_tc = wx.TextCtrl(self.panel, -1, "", size=size, style=wx.TE_PROCESS_ENTER)
        self.w_prefix_tc.SetInsertionPoint(0)

        self.gpu_rate_t = wx.StaticText(self.panel, -1, "GPU rate:", size=size)
        self.gpu_rate_tc = wx.TextCtrl(self.panel, -1, "", size=size, style=wx.TE_PROCESS_ENTER)
        self.gpu_rate_tc.SetInsertionPoint(0)

        self.test_num_t = wx.StaticText(self.panel, -1, "Total test texts:", size=size)
        self.test_num_tc = wx.TextCtrl(self.panel, -1, "", size=size, style=wx.TE_PROCESS_ENTER)
        self.test_num_tc.SetInsertionPoint(0)

        self.texts_amount_t = wx.StaticText(self.panel, -1, "Total texts:", size=size)
        self.texts_amount_tc = wx.TextCtrl(self.panel, -1, "", size=size, style=wx.TE_PROCESS_ENTER)
        self.texts_amount_tc.SetInsertionPoint(0)

        self.load_w_t = wx.StaticText(self.panel, -1, "Load weights:", size=size)
        self.load_w_button = wx.Button(self.panel, -1, "Choose", size=size)
        self.Bind(wx.EVT_BUTTON, self.Load_Weights, self.load_w_button)

        self.set_def_but = wx.Button(self.panel, -1, "Default", size=size)
        self.Bind(wx.EVT_BUTTON, self.SetDefault, self.set_def_but)
        self.save_button = wx.Button(self.panel, -1, "Save&Exit", size=size)
        self.Bind(wx.EVT_BUTTON, self.OnSave, self.save_button)


    def Load_Weights(self, event):
        dialog = wx.DirDialog(None, "Choose a weights fold:", './models/DPR_model/',
            style=wx.DD_DEFAULT_STYLE|wx.DD_NEW_DIR_BUTTON)
        if dialog.ShowModal() == wx.ID_OK:
            self.weights_dir = dialog.GetPath()
        dialog.Destroy() 


    def SetDefault(self, event):
        self.args.vec_length = 50
        self.args.wordvec = 100
        self.args.actionDB = 'tag_actions1'
        self.args.replay_size = 10000
        self.args.words_num = 500
        self.args.num_actions = 1000
        self.args.batch_size = 8
        self.args.epochs = 2
        self.args.save_weights_prefix = './models/DPR_model/tb0'
        self.args.gpu_rate = 0.2
        self.args.use_gpu = 1
        self.args.test_text_num = 20
        self.args.max_text_num = 2
        self.test_one = 0
        self.args.load_weights = './models/DPR_model/tb0_1.prm'

        self.vec_len_tc.SetValue('50')
        self.actionDB_tc.SetValue('tag_actions1')
        self.replay_size_tc.SetValue('10000')
        self.words_num_tc.SetValue('500')
        self.batch_size_tc.SetValue('8')
        self.epochs_tc.SetValue('2')
        self.w_prefix_tc.SetValue('./models/DPR_model/tb0')
        self.gpu_rate_tc.SetValue('0.2')
        self.test_num_tc.SetValue('2')
        self.texts_amount_tc.SetValue('20')


    def OnSave(self, event):
        vl = self.vec_len_tc.GetValue().encode('utf-8').split()
        if vl:
            self.args.vec_length = int(vl[0])
            self.args.wordvec = 2*self.args.vec_length
        ad = self.actionDB_tc.GetValue().encode('utf-8').split()
        if ad:
            self.args.actionDB = ad[0]
        rs = self.replay_size_tc.GetValue().encode('utf-8').split()
        if rs:
            self.args.replay_size = int(rs[0])
        wn = self.words_num_tc.GetValue().encode('utf-8').split()
        if wn:
            self.args.words_num = int(wn[0])
            self.args.num_actions = 2*self.args.words_num
        bs = self.batch_size_tc.GetValue().encode('utf-8').split()
        if bs:
            self.args.batch_size = int(bs[0])
        ep = self.epochs_tc.GetValue().encode('utf-8').split()
        if ep:
            self.args.epochs = int(ep[0])
        wp = self.w_prefix_tc.GetValue().encode('utf-8').split()
        if wp:
            self.args.save_weights_prefix = wp[0]
        gpu = self.gpu_rate_tc.GetValue().encode('utf-8').split()
        if gpu:
            self.args.gpu_rate = float(gpu[0])
            if self.args.gpu_rate == 0:
                self.use_gpu = 0
        tn = self.test_num_tc.GetValue().encode('utf-8').split()
        if tn:
            self.args.test_text_num = int(tn[0])
        ta = self.texts_amount_tc.GetValue().encode('utf-8').split()
        if ta:
            self.args.max_text_num = ta[0]
        if self.weights_dir:
            self.args.load_weights = self.weights_dir
        self.Destroy()


       
class DUP_Settings(wx.Frame):
    def __init__(self, args):
        wx.Frame.__init__(self, None, -1, "DUP Model Settings")
        self.panel = wx.Panel(self)
        self.panel.SetBackgroundColour('khaki')
        self.args = args

        self.show_init()
        sub_box1 = wx.BoxSizer(wx.VERTICAL)
        sub_box2 = wx.BoxSizer(wx.VERTICAL)
        box = wx.BoxSizer(wx.HORIZONTAL)
        sub_box1.Add(self.sub_sub_box(self.k_t, self.k_tc))
        sub_box1.Add(self.sub_sub_box(self.iter_num_t, self.iter_num_tc))
        sub_box1.Add(self.sub_sub_box(self.learning_rate_t, self.learning_rate_tc))
        sub_box1.Add(self.sub_sub_box(self.blank_rate_t, self.blank_rate_tc))
        sub_box1.Add(self.sub_sub_box(self.pred_size_t, self.pred_size_tc))
        sub_box1.Add(self.sub_sub_box(self.domain_t, self.domain_button))
        sub_box2.Add(self.sub_sub_box(self.choose_text, self.train_wv_choice))
        sub_box2.Add(self.sub_sub_box(self.workers_t, self.workers_tc))
        sub_box2.Add(self.sub_sub_box(self.min_count_t, self.min_count_tc))
        sub_box2.Add(self.sub_sub_box(self.window_size_t, self.window_size_tc))
        sub_box2.Add(self.sub_sub_box(self.wv_iter_t, self.wv_iter_tc))
        sub_box2.Add(self.sub_sub_box(self.set_def_but, self.save_button))
        box.Add(sub_box1, 0, wx.ALL, 10)
        box.Add(sub_box2, 0, wx.ALL, 10)

        self.panel.SetSizer(box)
        box.Fit(self)


    def sub_sub_box(self, text, textctrl, style=wx.ALL|wx.EXPAND, gaps=5):
        box = wx.BoxSizer(wx.HORIZONTAL)
        box.Add(text, 0, style, gaps)
        box.Add(textctrl, 0, style, gaps)
        return box

    def show_init(self):
        size = (120, -1)
        self.k_t = wx.StaticText(self.panel, -1, "K-fold value:", size=size)
        self.k_tc = wx.TextCtrl(self.panel, -1, "", size=size, style=wx.TE_PROCESS_ENTER)
        self.k_tc.SetInsertionPoint(0)

        self.workers_t = wx.StaticText(self.panel, -1, "Workers:", size=size)
        self.workers_tc = wx.TextCtrl(self.panel, -1, "", size=size, style=wx.TE_PROCESS_ENTER)
        self.workers_tc.SetInsertionPoint(0)

        self.min_count_t = wx.StaticText(self.panel, -1, "Minimum count:", size=size)
        self.min_count_tc = wx.TextCtrl(self.panel, -1, "", size=size, style=wx.TE_PROCESS_ENTER)
        self.min_count_tc.SetInsertionPoint(0)

        self.window_size_t = wx.StaticText(self.panel, -1, "Window size:", size=size)
        self.window_size_tc = wx.TextCtrl(self.panel, -1, "", size=size, style=wx.TE_PROCESS_ENTER)
        self.window_size_tc.SetInsertionPoint(0)

        self.wv_iter_t = wx.StaticText(self.panel, -1, "Wordvec iters:", size=size)
        self.wv_iter_tc = wx.TextCtrl(self.panel, -1, "", size=size, style=wx.TE_PROCESS_ENTER)
        self.wv_iter_tc.SetInsertionPoint(0)

        self.iter_num_t = wx.StaticText(self.panel, -1, "DUP iters:", size=size)
        self.iter_num_tc = wx.TextCtrl(self.panel, -1, "", size=size, style=wx.TE_PROCESS_ENTER)
        self.iter_num_tc.SetInsertionPoint(0)

        self.learning_rate_t = wx.StaticText(self.panel, -1, "Learning rate:", size=size)
        self.learning_rate_tc = wx.TextCtrl(self.panel, -1, "", size=size, style=wx.TE_PROCESS_ENTER)
        self.learning_rate_tc.SetInsertionPoint(0)

        self.blank_rate_t = wx.StaticText(self.panel, -1, "Blank rate:", size=size)
        self.blank_rate_tc = wx.TextCtrl(self.panel, -1, "", size=size, style=wx.TE_PROCESS_ENTER)
        self.blank_rate_tc.SetInsertionPoint(0)

        self.pred_size_t = wx.StaticText(self.panel, -1, "Prediction size:", size=size)
        self.pred_size_tc = wx.TextCtrl(self.panel, -1, "", size=size, style=wx.TE_PROCESS_ENTER)
        self.pred_size_tc.SetInsertionPoint(0)

        self.domain_t = wx.StaticText(self.panel, -1, "Domain:", size=size)
        self.domain_button = wx.Button(self.panel, -1, "Choose", size=size)
        self.Bind(wx.EVT_BUTTON, self.Choose_DomainDir, self.domain_button)

        self.choose_text = wx.StaticText(self.panel, -1, "Train Wordvec", size=size)
        self.choose_text.SetFont(wx.Font(10, wx.DEFAULT, wx.NORMAL, wx.BOLD))
        self.train_wv_choice = wx.Choice(self.panel, -1, choices=['False', 'True'], size=size)

        self.set_def_but = wx.Button(self.panel, -1, "Default", size=size)
        self.Bind(wx.EVT_BUTTON, self.SetDefault, self.set_def_but)

        self.save_button = wx.Button(self.panel, -1, "Save&Exit", size=size)
        self.Bind(wx.EVT_BUTTON, self.OnSave, self.save_button)


    def Choose_DomainDir(self, event):
        dom_type = "Text source (*.txt)|*.txt|" "All files (*.*)|*.*"
        dialog = wx.FileDialog(None, "Choose a file", './domains/DUP/', "", dom_type, wx.OPEN)
        if dialog.ShowModal() == wx.ID_OK:
            self.args.domain = dialog.GetPath().encode('utf-8')
            self.domain = re.sub(r'.*/','', self.args.domain) 
            self.domain_button.SetLabelText(self.domain)
        dialog.Destroy()


    def SetDefault(self, event):
        self.args.domain_dir = "./domains/DUP/blocks/"
        self.args.domain = "blocks5000.txt"
        self.args.k = 10
        self.args.train_word2vec = 1
        self.args.workers = 4
        self.args.min_count = 1
        self.args.window_size = 1
        self.args.word2vec_iter = 20
        self.args.iter_num = 1
        self.args.learning_rate = 0.01
        self.args.blank_percentage = 0.05
        self.args.prediction_set_size = 10

        self.k_tc.SetValue('10')
        self.workers_tc.SetValue('4')
        self.min_count_tc.SetValue('1')
        self.window_size_tc.SetValue('1')
        self.wv_iter_tc.SetValue('20')
        self.iter_num_tc.SetValue('1')
        self.learning_rate_tc.SetValue('0.01')
        self.blank_rate_tc.SetValue('0.05')
        self.pred_size_tc.SetValue('10')
        self.train_wv_choice.SetSelection(1)


    def OnSave(self, event):
        kt = self.k_tc.GetValue().encode('utf-8').split()
        if kt:
            self.args.k = int(kt[0])

        itn = self.iter_num_tc.GetValue().encode('utf-8').split()
        if itn:
            self.args.iter_num = int(itn[0])

        lr = self.learning_rate_tc.GetValue().encode('utf-8').split()
        if lr:
            self.args.learning_rate = float(lr[0])

        br = self.blank_rate_tc.GetValue().encode('utf-8').split()
        if br:
            self.args.blank_percentage = float(br[0])

        ps = self.pred_size_tc.GetValue().encode('utf-8').split()
        if ps:
            self.args.prediction_set_size = int(ps[0])

        if self.train_wv_choice.GetSelection() == 1:
            ws = self.workers_tc.GetValue().encode('utf-8').split()
            if ws:
                self.args.workers = int(ws[0])

            mc = self.min_count_tc.GetValue().encode('utf-8').split()
            if mc:
                self.args.min_count = int(mc[0])

            wds = self.window_size_tc.GetValue().encode('utf-8').split()
            if wds:
                self.args.window_size = int(wds[0])

            wvi = self.wv_iter_tc.GetValue().encode('utf-8').split()
            if wvi:
                self.args.word2vec_iter = int(wvi[0])

        self.Destroy()

          



class MainWindow(wx.Frame):
    def __init__(self):
        wx.Frame.__init__(self, None, -1, "PlanTool Main Window")
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
        img3 = wx.Image('icons/planlab_logo.png', wx.BITMAP_TYPE_ANY)
        w2 = img3.GetWidth()
        h2 = img3.GetHeight()
        img4 = img3.Scale(w/2, h/2)
        self.logo2 = wx.StaticBitmap(self.panel, -1, wx.BitmapFromImage(img4))

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
        csm_but = buttons.GenButton(self.panel, -1, 'Case-Based Model-Lite Planning')
        self.Bind(wx.EVT_BUTTON, self.DeterministicTrack, det_but)
        self.Bind(wx.EVT_BUTTON, self.UncertaintyTrack, unc_but)
        self.Bind(wx.EVT_BUTTON, self.LearningTrack, lrn_but)
        self.Bind(wx.EVT_BUTTON, self.CaseBasedShallow, csm_but)
        box = wx.StaticBox(self.panel, -1, 'Applications')
        self.button_box = wx.StaticBoxSizer(box, wx.VERTICAL)
        self.button_box.Add(det_but, 0, wx.EXPAND|wx.ALL, 5)
        self.button_box.Add(unc_but, 0, wx.EXPAND|wx.ALL, 5)
        self.button_box.Add(lrn_but, 0, wx.EXPAND|wx.ALL, 5)
        self.button_box.Add(csm_but, 0, wx.EXPAND|wx.ALL, 5)
        self.button_box.Add(self.logo2, 0, wx.EXPAND|wx.ALL, 30)


    def set_gen_button(self, but_name, fs=8, fsty=wx.NORMAL, fity=wx.NORMAL, 
        fbold=wx.NORMAL, bw=0, bcolor='DEFAULT', fcolor='black', tips=''):
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
        self.child_frame = PlannerGUI("Deterministic\n   Planning", '', 'D')
        self.child_frame.Show()

    def UncertaintyTrack(self, event): pass
    def CaseBasedShallow(self, event): pass
    def LearningTrack(self, event): 
        self.child_frame = PlannerGUI("LearningTrack\nof Planning", 'aquamarine', 'L')
        self.child_frame.Show()


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
        wx.Exit()


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
        c_time = time.strftime('%Y-%m-%d %H:%M:%S',time.localtime(time.time()))
        #logging.debug( "\n%s\nInitializing MyApp..."%c_time )
        wx.App.__init__(self, redirect, filename)
        
    def OnInit(self):
        self.parent_frame = MainWindow()
        self.parent_frame.Show()
        #self.SetTopWindow(self.parent_frame)
        #print >> sys.stderr, "\n-----A pretend error message-----\n" 
        return True
    
    def OnExit(self):
        pass
        #logging.debug( "\nExiting Myapp...\n" )



        
if __name__ == '__main__':
    start_time = time.time()
    app = MyApp(redirect = False)
    #logging.debug( "Begin a MainLoop\n" )
    app.MainLoop()    
    #logging.debug( "Finish a MainLoop\n" )
