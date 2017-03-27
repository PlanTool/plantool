import wx

class SketchAbout(wx.Dialog):
    text = '''
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

    def __init__(self, parent):
        wx.Dialog.__init__(self, parent, -1, 'About Sketch', size=(440, 400) )
        html = wx.html.HtmlWindow(self)
        html.SetPage(self.text)
        button = wx.Button(self, wx.ID_OK, "Okay")
        sizer = wx.BoxSizer(wx.VERTICAL)
        sizer.Add(html, 1, wx.EXPAND|wx.ALL, 5)
        sizer.Add(button, 0, wx.ALIGN_CENTER|wx.ALL, 5)
        self.SetSizer(sizer)
        self.Layout()


def OnAbout(self, event):
    dlg = SketchAbout(self)
    dlg.ShowModal()
    dlg.Destroy()
