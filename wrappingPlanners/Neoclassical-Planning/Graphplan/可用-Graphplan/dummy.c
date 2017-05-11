/* contains dummy routines for compiling without certain features */
#include "graphplan.h"

void setup_viewer(void)
{ printf("Sorry, can't do X viewing in this version.\n"); }

void wait_until_left(void)
{ int i=0; }

void reset_viewer(int max_time)
{ int i = 0;}
void draw_fact(vertex_t v, int time, int flag)
{ int i=0; }
void draw_op(vertex_t v, int time, int flag, int thick)
{ int i=0; }

void do_final_viewing(void) {int i = 0; }

void do_graph_created() { int i=0; }

void handle_events() { int i=0; }
