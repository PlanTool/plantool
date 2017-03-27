# -*- coding: utf-8 -*-
#!/usr/bin/env python
import wx 
import wx.html 
import wx.lib.buttons as buttons


class SketchAbout(wx.Dialog):
    def __init__(self, parent):
        wx.Dialog.__init__(self, parent, -1, 'About Sketch', size=(440, 400) )
        html = wx.html.HtmlWindow(self)
        self.text = '''
        <html>
        <body bgcolor="#ACAA60">
        <center><table bgcolor="#455481" width="100%" cellspacing="0"
        cellpadding="0" border="1">
        <tr>
            <td align="center"><h1>Sketch!</h1></td>
        </tr>
        </table>
        </center>
        <p><b>Sketch</b> is a demonstration program for <b>wxPython In Action</b>
        Chapter 7. It is based on the SuperDoodle demo included with wxPython,
        available at http://www.wxpython.org/
        </p>

        <p><b>SuperDoodle</b> and <b>wxPython</b> are brought to you by
        <b>Robin Dunn</b> and <b>Total Control Software</b>, Copyright
        ? 1997-2006.</p>
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



class BlockWindow(wx.Panel): 
    def __init__(self, parent, ID=-1, label="", pos=wx.DefaultPosition, size=(100, 25)): 
        wx.Panel.__init__(self, parent, ID, pos, size, wx.RAISED_BORDER, label) 
        self.label = label 
        self.SetBackgroundColour("white") 
        self.SetMinSize(size) 
        #self.Bind(wx.EVT_PAINT, self.OnPaint)
         
    def OnPaint(self, evt): 
        sz = self.GetClientSize() 
        dc = wx.PaintDC(self) 
        w,h = dc.GetTextExtent(self.label) 
        dc.SetFont(self.GetFont()) 
        dc.DrawText(self.label, (sz.width-w)/2, (sz.height-h)/2) 


labels = "one two three four five six seven eight nine".split()

class TestFrame(wx.Frame):
    def __init__(self):
        wx.Frame.__init__(self, None, -1, "StaticBoxSizer Test")
        self.panel = wx.Panel(self)
        # make three static boxes with windows positioned inside them
        img1 = wx.Image('icons/planning0.gif', wx.BITMAP_TYPE_ANY)
        w = img1.GetWidth()
        h = img1.GetHeight()
        img2 = img1.Scale(w/1.5, h/1.5)
        sb1 = wx.StaticBitmap(self.panel, -1, wx.BitmapFromImage(img1))
        sb2 = wx.StaticBitmap(self.panel, -1, wx.BitmapFromImage(img2))


        #box1 = BlockWindow(self.panel, label='box1', size=(200,300))
        #box2 = self.MakeStaticBoxSizer("Box 2", labels[3:6])
        button1 = buttons.GenButton(self.panel, -1, 'About Button1')
        button2 = buttons.GenButton(self.panel, -1, 'Genric Button2')
        button3 = buttons.GenButton(self.panel, -1, 'Genric Button3')
        self.Bind(wx.EVT_BUTTON, self.OnAbout, button1)
        box = wx.StaticBox(self.panel, -1, 'Applications')
        inner_sizer = wx.StaticBoxSizer(box, wx.VERTICAL)
        inner_sizer.Add(button1, 0, wx.ALL, 2)
        inner_sizer.Add(button2, 0, wx.ALL, 2)
        inner_sizer.Add(button3, 0, wx.ALL, 2)

        # We can also use a sizer to manage the placement of other
        # sizers (and therefore the windows and sub-sizers that they
        # manage as well.)
        sizer = wx.BoxSizer(wx.HORIZONTAL)
        sizer.Add(sb1, 0, wx.ALL, 10)
        sizer.Add(sb2, 0, wx.ALL, 10)
        sizer.Add(inner_sizer, 0, wx.ALL, 10)
         
        self.panel.SetSizer(sizer)
        sizer.Fit(self)

    def MakeStaticBoxSizer(self, boxlabel, itemlabels):
        # first the static box
        box = wx.StaticBox(self.panel, -1, boxlabel)
        # then the sizer
        sizer = wx.StaticBoxSizer(box, wx.VERTICAL)
        # then add items to it like normal
        for label in itemlabels:
            bw = BlockWindow(self.panel, label=label)
            sizer.Add(bw, 0, wx.ALL, 2)
        return sizer

    def OnAbout(self, event):
        dlg = SketchAbout(self)
        dlg.ShowModal()
        dlg.Destroy()



class GridSizerFrame(wx.Frame):
    def __init__(self):
        wx.Frame.__init__(self, None, -1, "Basic Grid Sizer")
        sizer = wx.GridSizer(rows=3, cols=3, hgap=5, vgap=5)#创建grid sizer
        for label in labels:
            bw = BlockWindow(self, label=label)
            sizer.Add(bw, 0, 0)#添加窗口部件到sizer
        self.SetSizer(sizer)#把sizer与框架关联起来
        self.Fit()

app = wx.PySimpleApp()
TestFrame().Show()
#GridSizerFrame().Show()
app.MainLoop()



'''
class Example(wx.Frame):  
    def __init__(self,*args,**kw):  
        super(Example,self).__init__(*args,**kw)  
        self.InitUI()  
    def InitUI(self):  
        toolbar = self.CreateToolBar()  
        qtool = toolbar.AddLabelTool(wx.ID_ANY,"Quit",wx.Bitmap("exit.png"))  
        toolbar.Realize()  
          
        self.Bind(wx.EVT_TOOL, self.OnQuit, qtool)  
        self.SetSize((50,30))  
        self.Centre()  
        self.Show(True)  
    def OnQuit(self,e):  
        self.Close()  
def main():  
    ex = wx.App()  
    Example(None)  
    ex.MainLoop()  
if __name__ == '__main__':  
    main() 


if __name__ == "__main__":
    app = wx.PySimpleApp()
    dialog = wx.FontDialog(None, wx.FontData())
    if dialog.ShowModal() == wx.ID_OK:
        data = dialog.GetFontData()
        font = data.GetChosenFont()
        colour = data.GetColour()
        print 'colour:',colour
        print 'You selected: "%s", %d points\n'%(font.GetFaceName(), font.GetPointSize())
    dialog.Destroy()
    dialog = wx.ColourDialog(None, wx.ColourData())
    dialog.GetColourData().SetChooseFull(True)
    if dialog.ShowModal() == wx.ID_OK:
        data = dialog.GetColourData()
        print 'You selected: %s\n' % str(data.GetColour().Get())
    dialog.Destroy()




class ToolbarFrame(wx.Frame):
    def __init__(self, parent, id): 
        wx.Frame.__init__(self, parent, id, 'Toolbars',
        size=(300, 200))
        panel = wx.Panel(self)
        panel.SetBackgroundColour('White')  
        self.statusBar = self.CreateStatusBar() #1 创建状态栏 
        toolbar = self.CreateToolBar() #2 创建工具栏
        #toolbar.AddSimpleTool(wx.NewId(), images.getNewBitmap(),
        #"New", "Long help for 'New'") #3 给工具栏增加一个工具
        toolbar.Realize() #4 准备显示工具栏  
        menuBar = wx.MenuBar() # 创建菜单栏  
        # 创建两个菜单
        menu1 = wx.Menu()  
        menuBar.Append(menu1, "&File")  
        menu2 = wx.Menu()  
        #6 创建菜单的项目
        menu2.Append(wx.NewId(), "&Copy", "Copy in status bar")  
        menu2.Append(wx.NewId(), "C&ut", "")

        menu2.Append(wx.NewId(), "Paste", "")  
        menu2.AppendSeparator()  
        menu2.Append(wx.NewId(), "&Options...", "Display Options")
        menuBar.Append(menu2, "&Edit") # 在菜单栏上附上菜单  
        self.SetMenuBar(menuBar) # 在框架上附上菜单栏 


        self.button = wx.Button(panel, -1, "test", pos=(50,20))
        self.Bind(wx.EVT_BUTTON, self.test_button, self.button)
        self.button.SetDefault()
        self.count = 0


    def test_button(self, event):
        self.count += 1
        self.statusBar.SetStatusText('testing %d'%self.count)

    def menuData(self): #2 菜单数据
        return [("&File", 
                    (
                        ("&New", "New Sketch file", self.OnNew),
                        ("&Open", "Open sketch file", self.OnOpen),
                        ("&Save", "Save sketch file", self.OnSave),
                        ("&Quit", "Quit", self.OnCloseWindow),
                        ("", "", "")
                    )
                ),
                ("&Preferences",
                    (
                        ("&Color", 
                            (
                                ("&Red", "",    self.OnColor, wx.ITEM_RADIO),
                                ("&Orange", "", self.OnColor, wx.ITEM_RADIO),
                                ("&Yellow", "", self.OnColor, wx.ITEM_RADIO),
                                ("&Green", "",  self.OnColor, wx.ITEM_RADIO),
                                ("&Blue", "",  self.OnColor, wx.ITEM_RADIO)),
                            ("", "", "")
                        )
                    )
                )
                ]
    def menuData2(self): #2 菜单数据
        return [("&File", 
                    (
                        ("&New", "New Sketch file", self.OnNew),
                        ("&Open", "Open sketch file", self.OnOpen),
                        ("&Save", "Save sketch file", self.OnSave),
                        ("", "", ""),
                        ("&Color", 
                            (
                                ("&Red", "",    self.OnColor, wx.ITEM_RADIO),
                                ("&Orange", "", self.OnColor, wx.ITEM_RADIO),
                                ("&Yellow", "", self.OnColor, wx.ITEM_RADIO),
                                ("&Green", "",  self.OnColor, wx.ITEM_RADIO)
                            )
                        ),
                        ("", "", ""),
                        ("&Quit", "Quit", self.OnCloseWindow)
                    )
                )]


    def createMenuBar(self):
        self.menuBar = wx.MenuBar()
        for eachMenuData in self.menuData():
            menuLabel = eachMenuData[0]
            menuItems = eachMenuData[1]
            print '\nmenuLabel',menuLabel
            print '\nmenuItems',menuItems
            self.menuBar.Append(self.createMenu(menuItems), menuLabel)
        self.SetMenuBar(self.menuBar)



 
if __name__ == '__main__': 
    app = wx.PySimpleApp() 
    frame = ToolbarFrame(parent=None, id=-1)
    frame.Show()
    app.MainLoop()




#!/usr/bin/env python



    Function:简单的wxPython程序
    Input：NONE
    Output: NONE
    author: socrates
    blog:http://www.cnblogs.com/dyx1024/
    date:2012-07-01
      

import sys    
import wx


class MyFrame(wx.Frame):
    def __init__(self, parent, id, title):
        print u"Frame对象初始化(Frame _init__)"
        wx.Frame.__init__(self, parent, id, title)
              
class MyApp(wx.App):
    def __init__(self, redirect = True, filename = None):
        print "APP __init__"
        wx.App.__init__(self, redirect, filename)
        
    def OnInit(self):
        print u"APP对象的OnInit方法（OnInit）"
        self.frame = MyFrame(parent = None, id = -1, title = u"测试wxPyhont输出重定向")
        self.frame.Show()
        self.SetTopWindow(self.frame)
        print >> sys.stderr, u"输出到标准错误控制台。"
        return True
    
    def OnExit(self):
        print u"APP对象的OnExit方法"
        
    
def main():
    app = MyApp(redirect = True) #开始重定向
    print u"begin MainLoop"
    app.MainLoop()    
    print u"after MainLoop"
    
if __name__ == '__main__':
    main()



#coding:utf-8
import wx

class ListBoxFrame(wx.Frame):
    def __init__(self):
        wx.Frame.__init__(self, None, -1, 'List Box Example', size=(800, 600))
        self.panel = wx.Panel(self, -1)
        sampleList = ['zero', 'one', 'two', 'three', 'four', 'five',
        'six', 'seven', 'eight', 'nine', 'ten', 'eleven',
        'twelve', 'thirteen', 'fourteen']
        listBox = wx.ListBox(self.panel, -1, (20, 20), (80, 120), sampleList, wx.LB_SINGLE)
        listBox.SetSelection(3)
        self.gp_options_list = ['show irrelevants', 'Lower bound time', 
        'Build up to goals', 'no mutual exclusion', 'examine subsets']
        self.gp_options_choices = wx.CheckListBox(self.panel, -1, (120,100), (200,100),
            self.gp_options_list, wx.LB_MULTIPLE)
        #self.Bind(wx.EVT_LISTBOX, self.clb, self.gp_options_choices)

        self.button1 = wx.Button(self.panel, -1,"Test", pos=(100, 500))  
        self.Bind(wx.EVT_BUTTON, self.Test_Button, self.button1)
        self.button1.SetDefault()

    def Test_Button(self, event):
        print self.gp_options_choices.GetSelections()
        s = ''
        for i in range(len(self.gp_options_list)):
            if self.gp_options_choices.IsChecked(i):
                s += self.gp_options_list[i]
        print s
        #print help(self.gp_options_choices)

    def clb(self, event):
        print self.gp_options_choices.GetSelections()
 
if __name__ == '__main__':
    app = wx.PySimpleApp()
    ListBoxFrame().Show()
    app.MainLoop()  




if __name__ == "__main__":
    domainDir = '/home/fengwf/Documents/Planner/Domains/'
    app = wx.PySimpleApp()
    wildcard = "PDDL source (*.pddl)|*.pddl|" \
    "Text source (*.txt)|*.txt|" \
    "All files (*.*)|*.*"
    dialog = wx.FileDialog(None, "Choose a file", domainDir, "", 
        wildcard, wx.OPEN)
    if dialog.ShowModal() == wx.ID_OK:
        print dialog.GetPath() 
    dialog.Destroy()




    # 指定了前景色和背景色的静态文本
    rev = wx.StaticText(panel, -1, "Static Text With Reversed Colors",
    (100, 30))
    rev.SetForegroundColour('tan')
    rev.SetBackgroundColour('black')
    # 指定居中对齐的的静态文本
    center = wx.StaticText(panel, -1, "align center", (100, 50),
    (160, -1), wx.ALIGN_CENTER)
    center.SetForegroundColour('white')
    center.SetBackgroundColour('black')
    # 指定右对齐的静态文本
    right = wx.StaticText(panel, -1, "align right", (100, 70),
    (160, -1), wx.ALIGN_RIGHT)
    right.SetForegroundColour('white')
    right.SetBackgroundColour('black')
    # 指定新字体的静态文本
    str = "You can also change the font."
    text = wx.StaticText(panel, -1, str, (20, 100))
    font = wx.Font(18, wx.DECORATIVE, wx.ITALIC, wx.NORMAL)
    text.SetFont(font)
    # 显示多行文本
    wx.StaticText(panel, -1, "Your text\ncan be split\n"
    "over multiple lines\n\neven blank ones", (20,150))
    #显示对齐的多行文本
    wx.StaticText(panel, -1, "Multi-line text\ncan also\n"
    "be right aligned\n\neven with a blank", (220,150),
    style=wx.ALIGN_RIGHT)
    '''
