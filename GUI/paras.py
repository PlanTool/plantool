import wx

class PlannerWxParameters():
    def __init__(self, algName, names, tags, types, labels, values, defaults, outtag):
        self.all_types = ["choice", "text", "checkbox"]
        for type in types:
            if not type in self.all_types:
                raise RuntimeError("Error: undefined type.")
        self.labels = labels
        self.algName = algName
        self.types = types
        self.names = names
        self.tags = tags
        self.values = values
        self.defaults = defaults
        self.outtag = outtag




