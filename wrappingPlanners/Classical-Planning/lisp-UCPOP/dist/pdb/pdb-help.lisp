" (c) 1992-1995 Copyright (c) University of Washington
  Originally written by Tony Barrett. 
  Version (3.0) by David Christianson
  Version (4.0) enhancements and CLIM-2.0 conversion by Chung Kwok

  All rights reserved. Use of this software is permitted for non-commercial
  research purposes, and it may be copied only for that use.  All copies must
  include this copyright message.  This software is made available AS IS, and
  neither the authors nor the University of Washington make any warranty about
  the software or its performance.

  When you first acquire this software please send mail to
  bug-ucpop@cs.washington.edu; the same address should be used for problems."

;;; This file contains the data for PDB help facility.
;;; This file is generated from doc/pdb.html using lynx to print the page.
;;; 
(in-package 'vcr)

(defvar *pdb-help-text*
'((:about
"   
                             PDB: Plan debugger
                                       
   Version 4.0
   
   The work described in this document was based on the UCPOP planning
   system and applications developed by David Christianson, Tony Barrett,
   Chung Kwok, and J. Scott Penberthy at the University of Washington.
   This work was funded in part by Apple Computer.

"
)
  (:overview
"
Loading PDB

   
   
       
       Before you can use PDB, you must first initialize it using a
       special setup function:


 (pdb-setup &key display mono)

   This creates the structure which will hold all recorded planning
       sessions, configures the display for color or monochrome output,
       and launches the PDB process.

   

 Recording Plans From The Listener

   
   
       
       Sometimes it may be convenient to record plans from the LISP
       Listener. To start recording, use PDB-RECORD:


 (pdb-record &key (record-firings nil))

   Then invoke the UCPOP planner. To stop recording, use PDB-STOP:


 (pdb-stop)

   For your convenience, a number of commands have been defined to
       automatically perform these steps and then launch PDB. These
       commands take the same form as the standard UCPOP search
       routines(bf-control, ie-control, sc-control, etc.) but change the
       postfix to -show(bf-show, ie-show, sc-show, etc.). These functions
       start PDB, call the search function, then stop and display the
       result of planning.
")

  (:shell
"
The Plan System Overview Window

   
   
       
       Window Layout
       

       Menu Commands
       
          + Windows
               o Refresh Window: Redisplays the window contents.
               o Find Window: Displays a list of active subwindows.
                 Selecting a window will bring it to the front of the
                 screen.
               o Remove Window: Displays a list of active subwindows.
                 Selecting a window will cause it to be closed.
               o Remove All Subwindows: Closes all active subwindows.
               o Quit: Close this window.
          + Debug
               o New Session: Execute a new planning problem. This brings
                 up a dialog window containing the following options:
                    # Domain: Domain to take problems from.
                    # Problem: Problem to execute.
                    # Search Limit: Number of nodes to search before
                      stopping.
                    # Search Control: Yes or no. If yes, you must specify
                      a controller. If no, you must specify a search
                      function.
                    # Record Debugging: Yes if you want to record this as
                      a session.
                    # Record Firings: Yes if you want to record all
                      search control rule firings(slow).
               o New Browser: Browse domains, problems, and search
                 control.
               o Open Session: Open a VCR window on a session.
               o Delete Session: Delete a session: it will no longer
                 appear in the window.
               o Clear Session: Delete all sessions. All sessions will be
                 lost.
          + Options
               o Read Options: Reads back the defaults saved in
                 .plandebugrc
               o Write Options: Writes defaults out to .plandebugrc
   
       
       Plan System Overview Commands
       
          + Info Display the result of the current session.
          + VCR Display a trace of the current session.
          + Remove Remove the current session.
          + Recording Toggle plan recording on/off.
   
       
       Mouse Commands
       
          + Sessions
               o L: Find Window Clicking the left mouse button on a
                 session will select that session.
")
(:vcr   
"
   
The VCR Window

   
   
       
       Window Layout
       
       
       
       
       
       Menu Commands
       
          + Windows
               o Refresh Window: Redisplays the window contents.
               o Find Window: Displays a list of active subwindows.
                 Selecting a window will bring it to the front of the
                 screen.
               o Remove Window: Displays a list of active subwindows.
                 Selecting a window will cause it to be closed.
               o Remove All Subwindows: Closes all active subwindows.
               o Quit: Close this window.
          + Tree
               o Print: Print the current window. Asks for filename via
                 dialog.
               o All Nodes: Display all nodes in the tree.
               o Steps Only: Display nodes for plans with newly added
                 steps.
               o Searched Only: Display only those nodes that have been
                 searched.
               o Solution Only: Display solution node only (if found).
               o Solution Path Only: Display nodes on solution path (if
                 found).
               o Expand Node: Display the children of the current plan.
               o Contract Node: Do not display the children of the
                 current plan.
               o Expand Branch: Display the branch rooted at the current
                 plan.
               o Contract Branch: Do not display the branch rooted at the
                 current plan.
          + Options
               o View: Change the node picture of the tree nodes to one
                 of the following:
                    # Order: Color the nodes from dark to light green
                      depending on visitation order.
                    # Rank: Color the nodes from dark to light green
                      depending on the absolute rank.
               o Labels: Label the node branches with conditions branched
                 on:
                    # all: Label all branches.
                    # multiple: Label only branches with multiple
                      children.
                    # none: Don't label.
               o Resize: Shrink or enlarge the displayed search tree by a
                 constant factor.
   
       
       VCR Commands
       
          + : Backup the VCR until a mouse button is pressed.
        : Backup the VCR to the plan visited just prior to the current
            one.
        > : Progress the VCR to the next visited plan.
        >> : Progress the VCR until a mouse button is pressed. 
   
       
       Mouse Commands
       
          + Plan Tree Nodes
               o L: Select Node: Centers the cursor over this node. If
                 the cursor is already at this node, PDB will open a plan
                 window on that node.
               o M: Describe object: If Composer is running, PDB will
                 open a Composer window on the partial plan associated
                 with this node.
          + Plan Window Markers
               o L: Select Window: Moves the window to another node. This
                 is done by clicking once on the marker to move and once
                 on the node to move to.
   
       

       
       Viewing nodes by rank is not functional.
"       
)   
(:plan-window   
"   

The Plan Window

   
   
       
       Window Layout
       
       
       
       Menu Commands
       
          + Windows
               o Refresh Window: Redisplays the window contents.
               o Find Window: Displays a list of active subwindows.
                 Selecting a window will bring it to the front of the
                 screen.
               o Remove Window: Displays a list of active subwindows.
                 Selecting a window will cause it to be closed.
               o Remove All Subwindows: Closes all active subwindows.
               o Quit: Close this window.
          + Plan
               o Refine: Refine the current plan. Brings up a dialog with
                 search options.
                    # Search Limit: Number of nodes to search before
                      stopping.
                    # Choose Flaw: If yes, the user must select from a
                      list of current flaws. Search limit will be set to
                      1.
                    # Record Firings: Yes to record all search control
                      rule firings(slow).
               o Print: Print the current window. Asks for filename via
                 dialog.
               o Inspect: Inspect the current node.
          + Options
               o Graphics: Change the representation of the plan
                    # Graphics: Graphical, partial-order view.
                    # Text: Textual View
               o Verbosity: Control detail or representation
                    # Verbose: Print out conditions and steps fully
                    # Terse: Show only steps
   
       
       Mouse Commands
       
          + Plan Tree Nodes
               o L: Select Node: Pressing the left button on a node
                 causes the window to display the plan associated with
                 that node. The navigational display is updated, as is
                 the VCR window associated with the plan window.
               o M: Describe object: If Composer is running, a Composer
                 window will be opened on the plan associated with the
                 selected node.
          + Plan Structures
               o L: Choose Flaw: Pressing the left button on a flaw(shown
                 in red) will bring up a plan refinement dialog with the
                 selected flaw already chosen for refinement. Clicking
                 the left button on other structures has no effect.
               o M: Describe object: If Composer is running, a Composer
                 window will be opened on the structure associated with
                 the highlighted graphic.
               o Shift-L: Move Object: Pressing Shift-L over a step in
                 graphic display mode will let you drag the step to a new
                 position. This allows you to view the plan in a more
                 reasonable manner than the automatic layout provides.
"   
)   
(:browser   
"

Browser Window

   
   
       
       Window Layout
       
       
       
       Menu Commands
       
          + Window
               o Refresh Window: Redisplays the window contents.
               o Quit: Close this window.
          + Object
               o Print: Print the current window.
               o Inspect: Inspect the current object.
          + Options
   
       
       Browser Commands
       
          + Scroll the browser menus towards the top of the heirarchy.
        -> Scroll the browser menus towards the bottom of the heirarchy.
            
   
       
       Mouse Commands
       
          + L: Select Object: Clicking on an item in one of the browser
            menus will bring up the menu associated with that item(if
            any) in the pane to the right of the selection, as well as
            the object associated with that item(if any).
"   
 )))
