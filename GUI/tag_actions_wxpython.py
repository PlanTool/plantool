#coding:utf-8
import wx
import mysql.connector
import time
import re

'''
create table tag_actions4(text_num int, sent_num int, sent varchar(400), tag_sent varchar(200));
'''

class TextFrame(wx.Frame):
    def __init__(self):
        self.home_dir = "/media/Documents/cook_tutorial/DRL_data/cooking_tutorials/"
        self.table = 'tag_actions5'
        self.total_text = 64
        self.text_num = 0
        self.sent_num = 0
        self.num = 0
        self.max_tags = 0
        self.wt = []
        self.tag_sent = []
        self.display_sent = []
        self.start_flag = False
        self.search_flag = False
        self.delete_flag = False
        self.change_flag = False
        self.table_flag = False
        self.exit_flag = False
        
        wx.Frame.__init__(self, None, -1, 'Tag Actions from texts', size=(800, 600))  #整个界面大小
        panel = wx.Panel(self, -1)
        panel.SetBackgroundColour('aquamarine')
        panel.Refresh()
        
        str = "Welcome to the tag_actions.app from Fence!"  #静态显示文本，欢迎标题
        self.text = wx.StaticText(panel, -1, str, (140,-1))
        font1 = wx.Font(18, wx.ROMAN, wx.ITALIC, wx.BOLD)  #字体18号，罗马字，斜体，加粗
        self.text.SetFont(font1)
        self.text.SetForegroundColour('tan')  #颜色为棕色
        
        self.info_text = wx.StaticText(panel, -1, "All rights reserved"+u'©'+"fengwf.PlanLab.2016", (540,540)) #输入文本框
        self.info_text.SetForegroundColour('dark slate blue')

        self.OutputText = wx.TextCtrl(panel, -1,"Input text_num and sent_num:\n",
        size=(700, 400), pos=(50,40), style=wx.TE_MULTILINE | wx.TE_READONLY | wx.TE_RICH2) #创建一个输出文本控件
        font2 = wx.Font(12, wx.SCRIPT, wx.NORMAL, wx.NORMAL)
        font3 = wx.Font(12, wx.DEFAULT, wx.NORMAL, wx.NORMAL)
        self.OutputText.SetFont(font3)
        #self.OutputText.SetForegroundColour('orchid')
        self.pointer = self.OutputText.GetInsertionPoint()
        #print 'self.pointer',self.pointer
        
        self.InputLabel = wx.StaticText(panel, -1, "Your input:", (100,455)) #输入文本框
        self.InputText = wx.TextCtrl(panel, -1, "", size=(175, -1), \
        style=wx.TE_PROCESS_ENTER, pos=(180,450))
        self.InputText.SetInsertionPoint(0)
        self.Bind(wx.EVT_TEXT_ENTER,self.OnText,self.InputText)
        
        self.SearchLabel = wx.StaticText(panel, -1, "Search input:", (390,455)) #输入文本框
        self.SearchText = wx.TextCtrl(panel, -1, "", size=(175, -1), \
        style=wx.TE_PROCESS_ENTER, pos=(480,450))  #查询的输入文本框
        self.SearchText.SetInsertionPoint(0)
        self.Bind(wx.EVT_TEXT_ENTER,self.OnText2,self.SearchText)

        self.button1 = wx.Button(panel, -1,"Search", pos=(100, 500))  #搜索按钮
        self.Bind(wx.EVT_BUTTON, self.Search_Button, self.button1)
        self.button1.SetDefault()
        
        self.button2 = wx.Button(panel, -1,"A", pos=(180, 500))  #按钮，显示全部记录
        self.Bind(wx.EVT_BUTTON, self.ShowAll_Button, self.button2)
        self.button2.SetDefault()
        
        self.button3 = wx.Button(panel, -1,"L", pos=(260, 500))  #按钮，显示最近十条记录
        self.Bind(wx.EVT_BUTTON, self.ShowLast10_Button, self.button3)
        self.button3.SetDefault()
        
        self.button4 = wx.Button(panel, -1,"D", pos=(340, 500))  #按钮，删除记录
        self.Bind(wx.EVT_BUTTON, self.Delete_Button, self.button4)
        self.button4.SetDefault()
        
        self.button5 = wx.Button(panel, -1,"Exit", pos=(500, 500))  #按钮，退出
        self.Bind(wx.EVT_BUTTON, self.Exit_Button, self.button5)
        self.button5.SetDefault()
        
        self.button6 = wx.Button(panel, -1,"Reboot", pos=(420, 500))  #按钮，退出
        self.Bind(wx.EVT_BUTTON, self.Reboot_Button, self.button6)
        self.button6.SetDefault()
        
        self.button7 = wx.Button(panel, -1,"ChangeDir", pos=(580, 500))  #按钮，退出
        self.Bind(wx.EVT_BUTTON, self.Change_dir_Button, self.button7)
        self.button7.SetDefault()
        
    def div_sents(self):
        try:
            t = open(self.home_dir+str(self.text_num+1)+".txt").read() 
        except IOError:
            self.OutputText.AppendText('\Could not open, no such file or directory as: %s\n'%self.home_dir)
        t = re.sub(r'\n',' ',t)
        t = re.sub(r',|"|\(|\)|\[|\]|\{|\}','',t)
        self.wt = []
        sents = re.split(r'[\.?!:;]+ ',t)  #分成每个句子，分号冒号也算完成一个句子
        for i in sents:
            words = i.split()
            if len(words) > 0:
                words[0] = words[0].lower()
                self.wt.append(words)
        return self.wt

    
    def OnText(self, event):
        s = self.InputText.GetValue().encode("ascii")
        temp_in = s.split()
        self.OutputText.AppendText(s+'\n')
        if not self.start_flag:
            if len(temp_in)!=2:
                self.pointer = self.OutputText.GetInsertionPoint()
                self.OutputText.AppendText('\nText_num out of range. Please input again:\n')
                tp_point = self.pointer
                self.pointer = self.OutputText.GetInsertionPoint()
                self.OutputText.SetStyle(tp_point, self.pointer, wx.TextAttr("red", wx.NullColour))  #设定颜色
            else:
                start_text = int(temp_in[0])
                start_sent = int(temp_in[1])
                if start_text < 0 or start_text > self.total_text-1:                    
                    self.pointer = self.OutputText.GetInsertionPoint()
                    self.OutputText.AppendText('\nText_num out of range. Please input again:\n')
                    tp_point = self.pointer
                    self.pointer = self.OutputText.GetInsertionPoint()
                    self.OutputText.SetStyle(tp_point, self.pointer, wx.TextAttr("red", wx.NullColour))  #设定颜色
                else:
                    self.start_flag = True
                    #self.OutputText.AppendText('self.start_flag = True\n')
                    self.text_num = start_text
                    self.sent_num = start_sent
                    self.div_sents()  #把文本分割成单词
                    #print 'len(wt)',len(self.wt)
                    if self.sent_num >= len(self.wt) or self.sent_num < 0:
                        self.sent_num = 0
                    self.pointer = self.OutputText.GetInsertionPoint()
                    self.OutputText.AppendText("\n\n***** Text name: %s *****" % (self.home_dir+str(self.text_num+1)+'.txt'))
                    self.OutputText.AppendText("\nTotal sentences: %d          Total words: %d\n" % (len(self.wt),sum(len(s) for s in self.wt)))
                    tp_point = self.pointer
                    self.pointer = self.OutputText.GetInsertionPoint()
                    self.OutputText.SetStyle(tp_point, self.pointer, wx.TextAttr("maroon", wx.NullColour))  #设定颜色
                    
                    self.display_sent = []
                    self.max_tags = len(self.wt[self.sent_num])
                    #print 'self.wt[self.sent_num]',self.wt[self.sent_num]
                    self.tag_sent = ['0' for col in range(self.max_tags)]
                    for jj in range(self.max_tags):
                        self.display_sent.append(self.wt[self.sent_num][jj]+'('+str(jj+1)+')')
                    self.OutputText.AppendText('\nSentence'+str(self.sent_num)+':'+' '.join(self.display_sent)+'\n')  #输出每个单词及其序号                    
                    tp_point = self.pointer
                    self.pointer = self.OutputText.GetInsertionPoint()
                    self.OutputText.SetStyle(tp_point, self.pointer, wx.TextAttr("blue", wx.NullColour))
                    self.OutputText.AppendText("Input the indexs and amount of actions in this sentence:\n")
        
        else:
            #self.OutputText.AppendText('Tagging...\n')
            if not len(temp_in):
                self.num = 0
            else:
                self.num = int(temp_in[-1])
                tags = [int(tag) for tag in temp_in[:-1]] #tag在前面，最后一位是tag的数量
                #print 'tags',tags,'len(tags)',len(tags)
                #print 'self.num',self.num
                #self.OutputText.AppendText(str(temp_in)+'\n')
                if self.num != -2 and self.num != len(tags):
                    self.num = -3
                    tags = []
            if self.num < 0 or self.num > self.max_tags:
                self.pointer = self.OutputText.GetInsertionPoint()
                self.OutputText.AppendText("Wrong tags!!! Please input again:\n")
                tp_point = self.pointer
                self.pointer = self.OutputText.GetInsertionPoint()
                self.OutputText.SetStyle(tp_point, self.pointer, wx.TextAttr("red", wx.NullColour))  #设定颜色
            else:
                if self.num == 0:
                    self.OutputText.AppendText("\n----------No actions to be tag----------\n")
                    #insert = "insert into "+self.table+" values("+str(self.text_num)+','+str(self.sent_num)+',"'+' '.join(self.wt[self.sent_num])\
                    #    +'","'+' '.join(self.tag_sent)+'")'
                    #cur.execute(insert)
                    #db.commit()
                else:
                    for kk in range(self.num):
                        self.tag_sent[tags[kk]-1] = '1'  #标记动作
                        self.OutputText.AppendText('\nTagged actions: '+str(self.wt[self.sent_num][tags[kk]-1])+' '+str(self.tag_sent[tags[kk]-1]))
                    insert = "insert into "+self.table+" values("+str(self.text_num)+','+str(self.sent_num)+',"'+' '.join(self.wt[self.sent_num])\
                        +'","'+' '.join(self.tag_sent)+'")'
                    self.OutputText.AppendText('\nTagged sentences: [{}]'.format(' '.join(self.tag_sent)))
                    cur.execute(insert)
                    db.commit()
                self.sent_num += 1
                #print 'self.sent_num',self.sent_num
                if self.sent_num >= len(self.wt):
                    self.text_num += 1
                    if self.text_num >= self.total_text:
                        end_time = time.time()
                        self.OutputText.AppendText('Total time cost: %ds'%(end_time-start_time))
                        print  'Total time cost: %ds'%(end_time-start_time)
                        wx.Exit()
                        self.exit_flag = True
                    else:
                        self.sent_num = 0
                        self.div_sents()
                        self.pointer = self.OutputText.GetInsertionPoint()
                        self.OutputText.AppendText("\n\n\n------------------------ A new text is starting... -----------------------")
                        self.OutputText.AppendText("\n***** Text name: %s *****" % (self.home_dir+str(self.text_num+1)+'.txt'))
                        self.OutputText.AppendText("\nTotal sentences: %d          Total words: %d\n" % (len(self.wt),sum(len(s) for s in self.wt)))
                        tp_point = self.pointer
                        self.pointer = self.OutputText.GetInsertionPoint()
                        self.OutputText.SetStyle(tp_point, self.pointer, wx.TextAttr("maroon", wx.NullColour))  #设定颜色
                if not self.exit_flag:
                    self.pointer = self.OutputText.GetInsertionPoint()
                    self.display_sent = []
                    self.max_tags = len(self.wt[self.sent_num])
                    self.tag_sent = ['0' for col in range(self.max_tags)]
                    for jj in range(self.max_tags):
                        self.display_sent.append(self.wt[self.sent_num][jj]+'('+str(jj+1)+')')
                    self.OutputText.AppendText('\n\nSentence'+str(self.sent_num)+':'+' '.join(self.display_sent)+'\n')  #输出每个单词及其序号                
                    tp_point = self.pointer
                    self.pointer = self.OutputText.GetInsertionPoint()
                    self.OutputText.SetStyle(tp_point, self.pointer, wx.TextAttr("blue", wx.NullColour))
                    self.OutputText.AppendText("Input the total actions and their indices:\n")
                
                
            
        self.InputText.Clear()
            
            
    def Search_Button(self, event):
        cur.execute('select * from '+self.table)#选择table tag_actions
        self.result = cur.fetchall()
        self.OutputText.AppendText("\nTotal records: %d"%len(self.result))
        self.OutputText.AppendText("\nHow many records do you want to display: \n A for all records \n L for last ten records \n D for delete last record\n ")
        self.search_flag = True
            
    def ShowAll_Button(self, event):
        self.OutputText.AppendText("\n-------------------------All Records In The Database-------------------------")
        self.OutputText.AppendText("\nTotal records: %d"%len(self.result))
        for detail in self.result:
            self.OutputText.AppendText('\n'+str(detail))
        self.OutputText.AppendText("\nTotal records: %d"%len(self.result))
        self.OutputText.AppendText("\n-------------------------------End Of Display--------------------------------")
        self.search_flag = False
        
        if self.start_flag:
            self.pointer = self.OutputText.GetInsertionPoint()
            self.OutputText.AppendText('\nSentence'+str(self.sent_num)+':'+' '.join(self.display_sent)+'\n')  #输出每个单词及其序号            
            tp_point = self.pointer
            self.pointer = self.OutputText.GetInsertionPoint()
            self.OutputText.SetStyle(tp_point, self.pointer, wx.TextAttr("blue", wx.NullColour))
            self.OutputText.AppendText("Input the indexs and amount of actions in this sentence:\n")
        else:
            self.OutputText.AppendText("Input text_num and sent_num:\n")
    
    def ShowLast10_Button(self, event):
        self.OutputText.AppendText("\n----------------------------Last Ten Records-------------------------------")
        if len(self.result) >= 10:
            start = len(self.result)-10
        else:
            start = 0
        for di in range(start,len(self.result)):
            self.OutputText.AppendText('\n'+str(self.result[di]))
        self.OutputText.AppendText("\n-----------------------------End Of Display--------------------------------")
        self.search_flag = False
        
        if self.start_flag:
            self.pointer = self.OutputText.GetInsertionPoint()
            self.OutputText.AppendText('\nSentence'+str(self.sent_num)+':'+' '.join(self.display_sent)+'\n')  #输出每个单词及其序号            
            tp_point = self.pointer
            self.pointer = self.OutputText.GetInsertionPoint()
            self.OutputText.SetStyle(tp_point, self.pointer, wx.TextAttr("blue", wx.NullColour))
            self.OutputText.AppendText("Input the indexs and amount of actions in this sentence:\n")
        else:
            self.OutputText.AppendText("Input text_num and sent_num:\n")
    
    def Delete_Button(self, event):
        self.delete_flag = True
        self.pointer = self.OutputText.GetInsertionPoint()
        self.OutputText.AppendText("\nPlease input text_num and sent_num in the Search input box:\n")
        tp_point = self.pointer
        self.pointer = self.OutputText.GetInsertionPoint()
        self.OutputText.SetStyle(tp_point, self.pointer, wx.TextAttr("red", wx.NullColour))
    
    def OnText2(self, event):
        if self.delete_flag:
            s = self.SearchText.GetValue().encode("ascii")
            temp_in = s.split()
            if len(temp_in)!=2:
                self.pointer = self.OutputText.GetInsertionPoint()
                self.OutputText.AppendText('\nText_num out of range. Please input again:\n')
                tp_point = self.pointer
                self.pointer = self.OutputText.GetInsertionPoint()
                self.OutputText.SetStyle(tp_point, self.pointer, wx.TextAttr("red", wx.NullColour))
                self.SearchText.Clear()
            else:
                tn = temp_in[0]
                sn = temp_in[1]
                delete = "delete from "+self.table+" where text_num="+tn+" and sent_num="+sn
                self.OutputText.AppendText('\ndelete'+delete+'\n')
                cur.execute(delete)
                db.commit()
                cur.execute('select * from '+self.table)#选择table tag_actions
                result = cur.fetchall()
                self.OutputText.AppendText("\n----------------------------Last Ten Records-------------------------------")
                if len(result) >= 10:
                    start = len(result)-10
                else:
                    start = 0
                for di in range(start,len(result)):
                    self.OutputText.AppendText('\n'+str(result[di]))
                self.OutputText.AppendText("\n-----------------------------End Of Display--------------------------------")
                
                self.delete_flag = False
                self.start_flag = False
                self.SearchText.Clear()
                self.OutputText.AppendText("Input text_num and sent_num:\n")
                
        elif self.change_flag:
            s = self.SearchText.GetValue().encode("ascii")
            self.home_dir = s
            self.OutputText.AppendText('\nYour input: %s\n'%s)
            self.OutputText.AppendText('\nPlease input a new table_name and text_num in Search input box\n')
            self.change_flag = False
            self.SearchText.Clear()
        
        elif self.table_flag:
            s = self.SearchText.GetValue().encode("ascii").split()
            if len(s) != 2:
                self.OutputText.AppendText('\nWrong input!! Please input again.\n')
                self.SearchText.Clear()
            else:
                if s[0][0:-1] != 'tag_actions':
                    self.OutputText.AppendText('\nWrong table name!! Please input again.\n')
                    self.SearchText.Clear()
                else:
                    self.table = s[0]
                    self.total_text = int(s[1])
                    self.SearchText.Clear()
                    #print s
                    self.OutputText.AppendText('\nNew table_name: %s  total_text: %s\n'%(s[0],s[1]))
                    self.table_flag = False
                    self.text_num = 0
                    self.sent_num = 0
                    self.num = 0
                    self.max_tags = 0
                    self.wt = []
                    self.tag_sent = []
                    self.display_sent = []
                    self.start_flag = False
                    self.search_flag = False
                    self.delete_flag = False
                    self.pointer = self.OutputText.GetInsertionPoint()
                    self.OutputText.SetValue("*****System reboot finishd.*****\n")
                    self.OutputText.AppendText("Input text_num and sent_num:\n")
                    tp_point = self.pointer
                    self.pointer = self.OutputText.GetInsertionPoint()
                    self.OutputText.SetStyle(tp_point, self.pointer, wx.TextAttr("red", wx.NullColour))
        
        else:
            self.SearchText.Clear()
            self.pointer = self.OutputText.GetInsertionPoint()
            self.OutputText.AppendText("\nYou should not input here now!\n")
            tp_point = self.pointer
            self.pointer = self.OutputText.GetInsertionPoint()
            self.OutputText.SetStyle(tp_point, self.pointer, wx.TextAttr("red", wx.NullColour))

    def Change_dir_Button(self, event):
        self.OutputText.AppendText('\nCurrent home_dir is %s\n' % self.home_dir)
        self.OutputText.AppendText('\nPlease input a new home_dir in Search input box\n')
        self.change_flag = True
        self.table_flag = True

    def Exit_Button(self, event):
        print '\nText_num: %d; Sentence_num: %d' %(self.text_num,self.sent_num)
        end_time = time.time()
        self.OutputText.AppendText('\nText_num: %d; Sentence_num: %d' %(self.text_num,self.sent_num))
        self.OutputText.AppendText('Total time cost: %ds'%(end_time-start_time))
        print  'Total time cost: %ds'%(end_time-start_time)
        wx.Exit()

    def Reboot_Button(self, event):
        self.text_num = 0
        self.sent_num = 0
        self.num = 0
        self.max_tags = 0
        self.wt = []
        self.tag_sent = []
        self.display_sent = []
        self.start_flag = False
        self.search_flag = False
        self.delete_flag = False
        self.pointer = self.OutputText.GetInsertionPoint()
        self.OutputText.SetValue("*****System reboot finishd.*****\n")
        self.OutputText.AppendText("Input text_num and sent_num:\n")
        tp_point = self.pointer
        self.pointer = self.OutputText.GetInsertionPoint()
        self.OutputText.SetStyle(tp_point, self.pointer, wx.TextAttr("red", wx.NullColour))
        
        
if __name__ == '__main__':
    start_time = time.time()
    db = mysql.connector.connect(user='fengwf',password='123',database='test')
    cur = db.cursor()
    app = wx.PySimpleApp()
    frame = TextFrame()
    frame.Show()
    app.MainLoop()