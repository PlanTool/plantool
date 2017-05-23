#######################################################################
#
# Author: Gabi Roeger
# Modified by: Silvia Richter (silvia.richter@nicta.com.au)
# (C) Copyright 2008: Gabi Roeger and NICTA
#
# This file is part of LAMA.
#
# LAMA is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the license, or (at your option) any later version.
#
# LAMA is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.
#
#######################################################################

import pddl_types

class Function(object):
    def __init__(self, name, arguments):
        self.name = name
        self.arguments = arguments
    def parse(alist):
        name = alist[0]
        arguments = pddl_types.parse_typed_list(alist[1:], functions=True)
        return Function(name, arguments)
    def parse_typed(alist, _type):
        function = Function.parse(alist)
        function.type = _type
        return function
    parse = staticmethod(parse)
    parse_typed = staticmethod(parse_typed)
    def __str__(self):
        if self.type:
            return "%s(%s): %s" % (self.name, ", ".join(map(str, self.arguments)), self.type) 
        else:
            return "%s(%s)" % (self.name, ", ".join(map(str, self.arguments))) 

