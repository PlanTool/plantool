/*********************************************************************

 * (C) Copyright 2008, University of Illinois, Urbana-Champaign

 *

 * All rights reserved. Use of this software is permitted ONLY for

 * non-commercial research purposes, and it may be copied only

 * for that use only. All copies must include this copyright message.

 * This software is made available AS IS, and neither the authors

 * nor the University of Illinois, make any warranty about the

 * software or its performance.

 *

 *********************************************************************/
/********************************************************************

 * File: dis_add_dp.c 

 * Authors:  Chih-wei Hsu and Benjamin W. Wah

 *

 *********************************************************************/
/*
# Copyright (C) 2004, Board of Trustees of the University of Illinois.
#
# The program is copyrighted by the University of Illinois, and should
# not be distributed without prior approval.  Commercialization of this 
# product requires prior licensing from the University of Illinois.
# Commercialization includes the integration of this code in part or 
# whole into a product for resale. 
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Author: Y. X. Chen, C. W. Hsu, and B. W. Wah, University of Illinois
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
*/
#include "dis_ff.h"
#include "dis_memory.h"

void add_new_predicates()
{
	dis_TypedListList *tll;
	
	/* NOT-CLOSED */
	tll = dis_new_dis_TypedListList();
	tll->predicate = dis_new_dis_Token( strlen( "NOT-CLOSED" ) + 1);
	strcpy(tll->predicate, "NOT-CLOSED");
	tll->args = dis_new_dis_TypedList();
	tll->args->name = dis_new_dis_Token( strlen("?X")+1 );
	strcpy(tll->args->name, "?X");
	tll->args->type = dis_new_dis_TokenList();
	tll->args->type->item = dis_new_dis_Token( strlen("DEVICE") + 1);
	strcpy(tll->args->type->item, "DEVICE");
	tll->next = dis_gparse_predicates;
	dis_gparse_predicates = tll;
	
	/* NOT-UNSAFE */
	tll = dis_new_dis_TypedListList();
	tll->predicate = dis_new_dis_Token( strlen( "NOT-UNSAFE" ) + 1);
	strcpy(tll->predicate, "NOT-UNSAFE");
	tll->args = dis_new_dis_TypedList();
	tll->args->name = dis_new_dis_Token( strlen("?X")+1 );
	strcpy(tll->args->name, "?X");
	tll->args->type = dis_new_dis_TokenList();
	tll->args->type->item = dis_new_dis_Token( strlen("DEVICE") + 1);
	strcpy(tll->args->type->item, "DEVICE");
	tll->args->next = dis_new_dis_TypedList();
	tll->args->next->name = dis_new_dis_Token( strlen("?SX")+1 );
	strcpy(tll->args->next->name, "?SX");
	tll->args->next->type = dis_new_dis_TokenList();
	tll->args->next->type->item = dis_new_dis_Token( strlen("SIDE") + 1);
	strcpy(tll->args->next->type->item, "SIDE");
	tll->next = dis_gparse_predicates;
	dis_gparse_predicates = tll;

	/* NOT-AFFECTED */
	tll = dis_new_dis_TypedListList();
	tll->predicate = dis_new_dis_Token( strlen( "NOT-AFFECTED" ) + 1);
	strcpy(tll->predicate, "NOT-AFFECTED");
	tll->args = dis_new_dis_TypedList();
	tll->args->name = dis_new_dis_Token( strlen("?X")+1 );
	strcpy(tll->args->name, "?X");
	tll->args->type = dis_new_dis_TokenList();
	tll->args->type->item = dis_new_dis_Token( strlen("DEVICE") + 1);
	strcpy(tll->args->type->item, "DEVICE");
	tll->next = dis_gparse_predicates;
	dis_gparse_predicates = tll;
}
void add_new_derived_predicates()
{
	char temp[64];
	dis_Pldis_Operator *scur_op;
	dis_PlNode *pl1, *pl2, *pl3, *pl4, *pl5, *pl6, *pl7, *pl8, *pl9, *pl10;
	dis_PlNode *pl11, *pl12, *pl13, *pl14, *pl15, *pl16, *pl17, *pl18, *pl19;
	dis_PlNode *pl20, *pl21, *pl22, *pl23, *pl24, *pl25, *pl26, *pl27, *pl28;
	
	/* NOT-AFFECTED */
	sprintf(temp, "deripred-%d>NOT-AFFECTED", ++dis_gnum_deripreds);
	scur_op = dis_new_dis_Pldis_Operator(temp);

	scur_op->number_of_real_params = 1;
	scur_op->parse_params = dis_new_dis_TypedList();;
	scur_op->parse_params->name = dis_new_dis_Token( strlen("?X")+1 );
	strcpy(scur_op->parse_params->name, "?X");
	scur_op->parse_params->type = dis_new_dis_TokenList();
	scur_op->parse_params->type->item = dis_new_dis_Token(strlen("DEVICE")+1);
	strcpy(scur_op->parse_params->type->item, "DEVICE");
	
	pl2 = dis_new_dis_PlNode(dis_ATOM);
	pl2->atom = dis_new_dis_TokenList();
	pl2->atom->item = dis_new_dis_Token(strlen("NOT-UNSAFE")+1);
	strcpy(pl2->atom->item, "NOT-UNSAFE");
	pl2->atom->next = dis_new_dis_TokenList();
	pl2->atom->next->item = dis_new_dis_Token(strlen("?X")+1);
	strcpy(pl2->atom->next->item, "?X");
	pl2->atom->next->next = dis_new_dis_TokenList();
	pl2->atom->next->next->item = dis_new_dis_Token(strlen("?SX")+1);
	strcpy(pl2->atom->next->next->item, "?SX");
	
	pl3 = dis_new_dis_PlNode(dis_ALL);
	pl3->parse_vars = dis_new_dis_TypedList();
	pl3->parse_vars->name = dis_new_dis_Token( strlen("?SX")+1 );
	strcpy(pl3->parse_vars->name, "?SX");
	pl3->parse_vars->type = dis_new_dis_TokenList();
	pl3->parse_vars->type->item = dis_new_dis_Token( strlen("SIDE") + 1);
	strcpy(pl3->parse_vars->type->item, "SIDE");
	pl3->atom = dis_new_dis_TokenList();
	pl3->atom->item = dis_new_dis_Token( strlen("?SX")+1 );
	strcpy(pl3->atom->item, "?SX");
	pl3->atom->next = dis_new_dis_TokenList();
	pl3->atom->next->item = dis_new_dis_Token( strlen("SIDE") + 1);
	strcpy(pl3->atom->next->item, "SIDE");
	pl3->sons = pl2;
	
	pl4 = dis_new_dis_PlNode(dis_ATOM);
	pl4->atom = dis_new_dis_TokenList();
	pl4->atom->item = dis_new_dis_Token(strlen("BREAKER")+1);
	strcpy(pl4->atom->item, "BREAKER");
	pl4->atom->next = dis_new_dis_TokenList();
	pl4->atom->next->item = dis_new_dis_Token(strlen("?X")+1);
	strcpy(pl4->atom->next->item, "?X");
	
	pl5 = dis_new_dis_PlNode(dis_NOT);
	pl5->sons = pl4;
	
	scur_op->preconds = dis_new_dis_PlNode(dis_OR);
	scur_op->preconds->sons = pl5;
	scur_op->preconds->sons->next = pl3;

	pl1 = dis_new_dis_PlNode(dis_ATOM);
	pl1->atom = dis_new_dis_TokenList();
	pl1->atom->item = dis_new_dis_Token(strlen("NOT-AFFECTED")+1);
	strcpy(pl1->atom->item, "NOT-AFFECTED");
	pl1->atom->next = dis_new_dis_TokenList();
	pl1->atom->next->item = dis_new_dis_Token(strlen("?X")+1);
	strcpy(pl1->atom->next->item, "?X");
	scur_op->effects = dis_new_dis_PlNode(dis_AND);
	scur_op->effects->sons = pl1;
	                        
	scur_op->next = dis_gloaded_ops;
	dis_gloaded_ops = scur_op;

	/* NOT-UNSAFE */
	sprintf(temp, "deripred-%d>NOT-UNSAFE", ++dis_gnum_deripreds);
	scur_op = dis_new_dis_Pldis_Operator(temp);

	scur_op->number_of_real_params = 2;
	scur_op->parse_params = dis_new_dis_TypedList();;
	scur_op->parse_params->name = dis_new_dis_Token( strlen("?X")+1 );
	strcpy(scur_op->parse_params->name, "?X");
	scur_op->parse_params->type = dis_new_dis_TokenList();
	scur_op->parse_params->type->item = dis_new_dis_Token(strlen("DEVICE")+1);
	strcpy(scur_op->parse_params->type->item, "DEVICE");
	scur_op->parse_params->next = dis_new_dis_TypedList();;
	scur_op->parse_params->next->name = dis_new_dis_Token( strlen("?SX")+1 );
	strcpy(scur_op->parse_params->next->name, "?SX");
	scur_op->parse_params->next->type = dis_new_dis_TokenList();
	scur_op->parse_params->next->type->item = dis_new_dis_Token(strlen("SIDE")+1);
	strcpy(scur_op->parse_params->next->type->item, "SIDE");

	pl1 = dis_new_dis_PlNode(dis_ATOM);
	pl1->atom = dis_new_dis_TokenList();
	pl1->atom->item = dis_new_dis_Token(strlen("NOT-UNSAFE")+1);
	strcpy(pl1->atom->item, "NOT-UNSAFE");
	pl1->atom->next = dis_new_dis_TokenList();
	pl1->atom->next->item = dis_new_dis_Token(strlen("?Y")+1);
	strcpy(pl1->atom->next->item, "?Y");
	pl1->atom->next->next = dis_new_dis_TokenList();
	pl1->atom->next->next->item = dis_new_dis_Token(strlen("?SY")+1);
	strcpy(pl1->atom->next->next->item, "?SY");

	pl3 = dis_new_dis_PlNode(dis_ATOM);
	pl3->atom = dis_new_dis_TokenList();
	pl3->atom->item = dis_new_dis_Token(strlen("CON")+1);
	strcpy(pl3->atom->item, "CON");
	pl3->atom->next = dis_new_dis_TokenList();
	pl3->atom->next->item = dis_new_dis_Token(strlen("?X")+1);
	strcpy(pl3->atom->next->item, "?X");
	pl3->atom->next->next = dis_new_dis_TokenList();
	pl3->atom->next->next->item = dis_new_dis_Token(strlen("SIDE2")+1);
	strcpy(pl3->atom->next->next->item, "SIDE2");
	pl3->atom->next->next->next = dis_new_dis_TokenList();
	pl3->atom->next->next->next->item = dis_new_dis_Token(strlen("?Y")+1);
	strcpy(pl3->atom->next->next->next->item, "?Y");
	pl3->atom->next->next->next->next = dis_new_dis_TokenList();
	pl3->atom->next->next->next->next->item = dis_new_dis_Token(strlen("?SY")+1);
	strcpy(pl3->atom->next->next->next->next->item, "?SY");
	
	pl5 = dis_new_dis_PlNode(dis_OR);
	pl5->sons = dis_new_dis_PlNode(dis_NOT);
	pl5->sons->sons = pl3;
	pl5->sons->next = pl1;

	pl7 = dis_new_dis_PlNode(dis_ALL);
	pl7->parse_vars = dis_new_dis_TypedList();
	pl7->parse_vars->name = dis_new_dis_Token( strlen("?SY")+1 );
	strcpy(pl7->parse_vars->name, "?SY");
	pl7->parse_vars->type = dis_new_dis_TokenList();
	pl7->parse_vars->type->item = dis_new_dis_Token( strlen("SIDE") + 1);
	strcpy(pl7->parse_vars->type->item, "SIDE");
	pl7->atom = dis_new_dis_TokenList();
	pl7->atom->item = dis_new_dis_Token( strlen("?SY")+1 );
	strcpy(pl7->atom->item, "?SY");
	pl7->atom->next = dis_new_dis_TokenList();
	pl7->atom->next->item = dis_new_dis_Token( strlen("SIDE") + 1);
	strcpy(pl7->atom->next->item, "SIDE");
	pl7->sons = pl5;
	
	pl9 = dis_new_dis_PlNode(dis_ALL);
	pl9->parse_vars = dis_new_dis_TypedList();
	pl9->parse_vars->name = dis_new_dis_Token( strlen("?Y")+1 );
	strcpy(pl9->parse_vars->name, "?Y");
	pl9->parse_vars->type = dis_new_dis_TokenList();
	pl9->parse_vars->type->item = dis_new_dis_Token( strlen("DEVICE") + 1);
	strcpy(pl9->parse_vars->type->item, "DEVICE");
	pl9->atom = dis_new_dis_TokenList();
	pl9->atom->item = dis_new_dis_Token( strlen("?Y")+1 );
	strcpy(pl9->atom->item, "?Y");
	pl9->atom->next = dis_new_dis_TokenList();
	pl9->atom->next->item = dis_new_dis_Token( strlen("DEVICE") + 1);
	strcpy(pl9->atom->next->item, "DEVICE");
	pl9->sons = pl7;
	
	pl11 = dis_new_dis_PlNode(dis_ATOM);
	pl11->atom = dis_new_dis_TokenList();
	pl11->atom->item = dis_new_dis_Token(strlen("FAULTY")+1);
	strcpy(pl11->atom->item, "FAULTY");
	pl11->atom->next = dis_new_dis_TokenList();
	pl11->atom->next->item = dis_new_dis_Token(strlen("?L")+1);
	strcpy(pl11->atom->next->item, "?L");

	pl13 = dis_new_dis_PlNode(dis_AND);
	pl13->sons = dis_new_dis_PlNode(dis_NOT);
	pl13->sons->sons = pl11;
	pl13->sons->next = pl9;
	
	pl15 = dis_new_dis_PlNode(dis_ATOM);
	pl15->atom = dis_new_dis_TokenList();
	pl15->atom->item = dis_new_dis_Token(strlen("EXT")+1);
	strcpy(pl15->atom->item, "EXT");
	pl15->atom->next = dis_new_dis_TokenList();
	pl15->atom->next->item = dis_new_dis_Token(strlen("?L")+1);
	strcpy(pl15->atom->next->item, "?L");
	pl15->atom->next->next = dis_new_dis_TokenList();
	pl15->atom->next->next->item = dis_new_dis_Token(strlen("?X")+1);
	strcpy(pl15->atom->next->next->item, "?X");
	pl15->atom->next->next->next = dis_new_dis_TokenList();
	pl15->atom->next->next->next->item = dis_new_dis_Token(strlen("SIDE2")+1);
	strcpy(pl15->atom->next->next->next->item, "SIDE2");
	
	pl17 = dis_new_dis_PlNode(dis_OR);
	pl17->sons = dis_new_dis_PlNode(dis_NOT);
	pl17->sons->sons = pl15;
	pl17->sons->next = pl13;
	
	pl19 = dis_new_dis_PlNode(dis_ALL);
	pl19->parse_vars = dis_new_dis_TypedList();
	pl19->parse_vars->name = dis_new_dis_Token( strlen("?L")+1 );
	strcpy(pl19->parse_vars->name, "?L");
	pl19->parse_vars->type = dis_new_dis_TokenList();
	pl19->parse_vars->type->item = dis_new_dis_Token( strlen("LINE") + 1);
	strcpy(pl19->parse_vars->type->item, "LINE");
	pl19->atom = dis_new_dis_TokenList();
	pl19->atom->item = dis_new_dis_Token( strlen("?L")+1 );
	strcpy(pl19->atom->item, "?L");
	pl19->atom->next = dis_new_dis_TokenList();
	pl19->atom->next->item = dis_new_dis_Token( strlen("LINE") + 1);
	strcpy(pl19->atom->next->item, "LINE");
	pl19->sons = pl17;
	
	pl21 = dis_new_dis_PlNode(dis_ATOM);
	pl21->atom = dis_new_dis_TokenList();
	pl21->atom->item = dis_new_dis_Token(strlen("=")+1);
	strcpy(pl21->atom->item, "=");
	pl21->atom->next = dis_new_dis_TokenList();
	pl21->atom->next->item = dis_new_dis_Token(strlen("?SX")+1);
	strcpy(pl21->atom->next->item, "?SX");
	pl21->atom->next->next = dis_new_dis_TokenList();
	pl21->atom->next->next->item = dis_new_dis_Token(strlen("SIDE1")+1);
	strcpy(pl21->atom->next->next->item, "SIDE1");

	pl23 = dis_new_dis_PlNode(dis_OR);
	pl23->sons = dis_new_dis_PlNode(dis_NOT);
	pl23->sons->sons = pl21;
	pl23->sons->next = pl19;

	pl2 = dis_new_dis_PlNode(dis_ATOM);
	pl2->atom = dis_new_dis_TokenList();
	pl2->atom->item = dis_new_dis_Token(strlen("NOT-UNSAFE")+1);
	strcpy(pl2->atom->item, "NOT-UNSAFE");
	pl2->atom->next = dis_new_dis_TokenList();
	pl2->atom->next->item = dis_new_dis_Token(strlen("?Y")+1);
	strcpy(pl2->atom->next->item, "?Y");
	pl2->atom->next->next = dis_new_dis_TokenList();
	pl2->atom->next->next->item = dis_new_dis_Token(strlen("?SY")+1);
	strcpy(pl2->atom->next->next->item, "?SY");

	pl4 = dis_new_dis_PlNode(dis_ATOM);
	pl4->atom = dis_new_dis_TokenList();
	pl4->atom->item = dis_new_dis_Token(strlen("CON")+1);
	strcpy(pl4->atom->item, "CON");
	pl4->atom->next = dis_new_dis_TokenList();
	pl4->atom->next->item = dis_new_dis_Token(strlen("?X")+1);
	strcpy(pl4->atom->next->item, "?X");
	pl4->atom->next->next = dis_new_dis_TokenList();
	pl4->atom->next->next->item = dis_new_dis_Token(strlen("SIDE1")+1);
	strcpy(pl4->atom->next->next->item, "SIDE1");
	pl4->atom->next->next->next = dis_new_dis_TokenList();
	pl4->atom->next->next->next->item = dis_new_dis_Token(strlen("?Y")+1);
	strcpy(pl4->atom->next->next->next->item, "?Y");
	pl4->atom->next->next->next->next = dis_new_dis_TokenList();
	pl4->atom->next->next->next->next->item = dis_new_dis_Token(strlen("?SY")+1);
	strcpy(pl4->atom->next->next->next->next->item, "?SY");
	
	pl6 = dis_new_dis_PlNode(dis_OR);
	pl6->sons = dis_new_dis_PlNode(dis_NOT);
	pl6->sons->sons = pl4;
	pl6->sons->next = pl2;

	pl8 = dis_new_dis_PlNode(dis_ALL);
	pl8->parse_vars = dis_new_dis_TypedList();
	pl8->parse_vars->name = dis_new_dis_Token( strlen("?SY")+1 );
	strcpy(pl8->parse_vars->name, "?SY");
	pl8->parse_vars->type = dis_new_dis_TokenList();
	pl8->parse_vars->type->item = dis_new_dis_Token( strlen("SIDE") + 1);
	strcpy(pl8->parse_vars->type->item, "SIDE");
	pl8->atom = dis_new_dis_TokenList();
	pl8->atom->item = dis_new_dis_Token( strlen("?SY")+1 );
	strcpy(pl8->atom->item, "?SY");
	pl8->atom->next = dis_new_dis_TokenList();
	pl8->atom->next->item = dis_new_dis_Token( strlen("SIDE") + 1);
	strcpy(pl8->atom->next->item, "SIDE");
	pl8->sons = pl6;
	
	pl10 = dis_new_dis_PlNode(dis_ALL);
	pl10->parse_vars = dis_new_dis_TypedList();
	pl10->parse_vars->name = dis_new_dis_Token( strlen("?Y")+1 );
	strcpy(pl10->parse_vars->name, "?Y");
	pl10->parse_vars->type = dis_new_dis_TokenList();
	pl10->parse_vars->type->item = dis_new_dis_Token( strlen("DEVICE") + 1);
	strcpy(pl10->parse_vars->type->item, "DEVICE");
	pl10->atom = dis_new_dis_TokenList();
	pl10->atom->item = dis_new_dis_Token( strlen("?Y")+1 );
	strcpy(pl10->atom->item, "?Y");
	pl10->atom->next = dis_new_dis_TokenList();
	pl10->atom->next->item = dis_new_dis_Token( strlen("DEVICE") + 1);
	strcpy(pl10->atom->next->item, "DEVICE");
	pl10->sons = pl8;
	
	pl12 = dis_new_dis_PlNode(dis_ATOM);
	pl12->atom = dis_new_dis_TokenList();
	pl12->atom->item = dis_new_dis_Token(strlen("FAULTY")+1);
	strcpy(pl12->atom->item, "FAULTY");
	pl12->atom->next = dis_new_dis_TokenList();
	pl12->atom->next->item = dis_new_dis_Token(strlen("?L")+1);
	strcpy(pl12->atom->next->item, "?L");

	pl14 = dis_new_dis_PlNode(dis_AND);
	pl14->sons = dis_new_dis_PlNode(dis_NOT);
	pl14->sons->sons = pl12;
	pl14->sons->next = pl10;
	
	pl16 = dis_new_dis_PlNode(dis_ATOM);
	pl16->atom = dis_new_dis_TokenList();
	pl16->atom->item = dis_new_dis_Token(strlen("EXT")+1);
	strcpy(pl16->atom->item, "EXT");
	pl16->atom->next = dis_new_dis_TokenList();
	pl16->atom->next->item = dis_new_dis_Token(strlen("?L")+1);
	strcpy(pl16->atom->next->item, "?L");
	pl16->atom->next->next = dis_new_dis_TokenList();
	pl16->atom->next->next->item = dis_new_dis_Token(strlen("?X")+1);
	strcpy(pl16->atom->next->next->item, "?X");
	pl16->atom->next->next->next = dis_new_dis_TokenList();
	pl16->atom->next->next->next->item = dis_new_dis_Token(strlen("SIDE1")+1);
	strcpy(pl16->atom->next->next->next->item, "SIDE1");
	
	pl18 = dis_new_dis_PlNode(dis_OR);
	pl18->sons = dis_new_dis_PlNode(dis_NOT);
	pl18->sons->sons = pl16;
	pl18->sons->next = pl14;
	
	pl20 = dis_new_dis_PlNode(dis_ALL);
	pl20->parse_vars = dis_new_dis_TypedList();
	pl20->parse_vars->name = dis_new_dis_Token( strlen("?L")+1 );
	strcpy(pl20->parse_vars->name, "?L");
	pl20->parse_vars->type = dis_new_dis_TokenList();
	pl20->parse_vars->type->item = dis_new_dis_Token( strlen("LINE") + 1);
	strcpy(pl20->parse_vars->type->item, "LINE");
	pl20->atom = dis_new_dis_TokenList();
	pl20->atom->item = dis_new_dis_Token( strlen("?L")+1 );
	strcpy(pl20->atom->item, "?L");
	pl20->atom->next = dis_new_dis_TokenList();
	pl20->atom->next->item = dis_new_dis_Token( strlen("LINE") + 1);
	strcpy(pl20->atom->next->item, "LINE");
	pl20->sons = pl18;
	
	pl22 = dis_new_dis_PlNode(dis_ATOM);
	pl22->atom = dis_new_dis_TokenList();
	pl22->atom->item = dis_new_dis_Token(strlen("=")+1);
	strcpy(pl22->atom->item, "=");
	pl22->atom->next = dis_new_dis_TokenList();
	pl22->atom->next->item = dis_new_dis_Token(strlen("?SX")+1);
	strcpy(pl22->atom->next->item, "?SX");
	pl22->atom->next->next = dis_new_dis_TokenList();
	pl22->atom->next->next->item = dis_new_dis_Token(strlen("SIDE2")+1);
	strcpy(pl22->atom->next->next->item, "SIDE2");

	pl24 = dis_new_dis_PlNode(dis_OR);
	pl24->sons = dis_new_dis_PlNode(dis_NOT);
	pl24->sons->sons = pl22;
	pl24->sons->next = pl20;

	pl25 = dis_new_dis_PlNode(dis_AND);
	pl25->sons = pl23;
	pl25->sons->next = pl24;

	pl26 = dis_new_dis_PlNode(dis_ATOM);
	pl26->atom = dis_new_dis_TokenList();
	pl26->atom->item = dis_new_dis_Token(strlen("NOT-CLOSED")+1);
	strcpy(pl26->atom->item, "NOT-CLOSED");
	pl26->atom->next = dis_new_dis_TokenList();
	pl26->atom->next->item = dis_new_dis_Token(strlen("?X")+1);
	strcpy(pl26->atom->next->item, "?X");
	
	pl27 = dis_new_dis_PlNode(dis_OR);
	pl27->sons = pl26;
	pl27->sons->next = pl25;
	scur_op->preconds = pl27;
	
	pl28 = dis_new_dis_PlNode(dis_ATOM);
	pl28->atom = dis_new_dis_TokenList();
	pl28->atom->item = dis_new_dis_Token(strlen("NOT-UNSAFE")+1);
	strcpy(pl28->atom->item, "NOT-UNSAFE");
	pl28->atom->next = dis_new_dis_TokenList();
	pl28->atom->next->item = dis_new_dis_Token(strlen("?X")+1);
	strcpy(pl28->atom->next->item, "?X");
	pl28->atom->next->next = dis_new_dis_TokenList();
	pl28->atom->next->next->item = dis_new_dis_Token(strlen("?SX")+1);
	strcpy(pl28->atom->next->next->item, "?SX");
	
	scur_op->effects = dis_new_dis_PlNode(dis_AND);
	scur_op->effects->sons = pl28;

	scur_op->next = dis_gloaded_ops;
	dis_gloaded_ops = scur_op;
}
void add_negated_effects_preconds()
{
	dis_Pldis_Operator *p;
	dis_PlNode *pl;
	
	for (p=dis_gloaded_ops;p;p=p->next)
	{
		if (strcasecmp(p->name, "WAIT") == 0)
		{
			pl = dis_new_dis_PlNode(dis_AND);
			pl->sons = p->effects->sons->sons->next;
			pl->sons->next = dis_new_dis_PlNode(dis_ATOM);
			pl->sons->next->atom = dis_new_dis_TokenList();
			pl->sons->next->atom->item = dis_new_dis_Token(strlen("NOT-CLOSED")+1);
			strcpy(pl->sons->next->atom->item, "NOT-CLOSED");
			pl->sons->next->atom->next = dis_new_dis_TokenList();
			pl->sons->next->atom->next->item = dis_new_dis_Token( strlen("?B")+1 );
			strcpy(pl->sons->next->atom->next->item, "?B");
			p->effects->sons->sons->next = pl;
		}
		else
		if (strcasecmp(p->name, "CLOSE") == 0)
		{
			pl = dis_new_dis_PlNode(dis_ATOM);
			pl->atom = dis_new_dis_TokenList();
			pl->atom->item = dis_new_dis_Token(strlen("NOT-AFFECTED")+1);
			strcpy(pl->atom->item, "NOT-AFFECTED");
			pl->atom->next = dis_new_dis_TokenList();
			pl->atom->next->item = dis_new_dis_Token( strlen("?B")+1 );
			strcpy(pl->atom->next->item, "?B");
			p->preconds->sons->next->next->sons = pl;
			
			pl = dis_new_dis_PlNode(dis_ATOM);
			pl->atom = dis_new_dis_TokenList();
			pl->atom->item = dis_new_dis_Token(strlen("NOT-CLOSED")+1);
			strcpy(pl->atom->item, "NOT-CLOSED");
			pl->atom->next = dis_new_dis_TokenList();
			pl->atom->next->item = dis_new_dis_Token(strlen("?X")+1);
			strcpy(pl->atom->next->item, "?X");
			pl->next = p->preconds->sons->next->next;
			p->preconds->sons->next = pl;
			
			pl = dis_new_dis_PlNode(dis_AND);
			pl->sons = p->effects;
			pl->sons->next = dis_new_dis_PlNode(dis_NOT);
			pl->sons->next->sons = dis_new_dis_PlNode(dis_ATOM);
			pl->sons->next->sons->atom = dis_new_dis_TokenList();
			pl->sons->next->sons->atom->item = dis_new_dis_Token(strlen("NOT-CLOSED")+1);
			strcpy(pl->sons->next->sons->atom->item, "NOT-CLOSED");
			pl->sons->next->sons->atom->next = dis_new_dis_TokenList();
			pl->sons->next->sons->atom->next->item = dis_new_dis_Token( strlen("?X")+1 );
			strcpy(pl->sons->next->sons->atom->next->item, "?X");
			p->effects = pl;
		}
		else
		if (strcasecmp(p->name, "OPEN") == 0)
		{
			pl = dis_new_dis_PlNode(dis_ATOM);
			pl->atom = dis_new_dis_TokenList();
			pl->atom->item = dis_new_dis_Token(strlen("NOT-AFFECTED")+1);
			strcpy(pl->atom->item, "NOT-AFFECTED");
			pl->atom->next = dis_new_dis_TokenList();
			pl->atom->next->item = dis_new_dis_Token( strlen("?B")+1 );
			strcpy(pl->atom->next->item, "?B");
			p->preconds->sons->next->next->sons = pl;

			pl = dis_new_dis_PlNode(dis_AND);
			pl->sons = p->effects;
			pl->sons->next = dis_new_dis_PlNode(dis_ATOM);
			pl->sons->next->atom = dis_new_dis_TokenList();
			pl->sons->next->atom->item = dis_new_dis_Token(strlen("NOT-CLOSED")+1);
			strcpy(pl->sons->next->atom->item, "NOT-CLOSED");
			pl->sons->next->atom->next = dis_new_dis_TokenList();
			pl->sons->next->atom->next->item = dis_new_dis_Token( strlen("?X")+1 );
			strcpy(pl->sons->next->atom->next->item, "?X");
			p->effects = pl;
		}
	}	
}
void init_and_goal()
{
	dis_PlNode *p, *pl;
	dis_TypedList *tl;

	/* init */
	for (tl=dis_gparse_objects;tl;tl=tl->next)
		if (strcasecmp(tl->type->item, "DEVICE") == 0)
		{
			for (p=dis_gorig_initial_facts->sons;p;p=p->next)
				if (p->connective == dis_ATOM && 
				strcasecmp(p->atom->item, "CLOSED") == 0 &&
				strcasecmp(p->atom->next->item, tl->name) == 0)
					break;
			if (p)
				continue;
			p = dis_new_dis_PlNode(dis_ATOM);
			p->atom = dis_new_dis_TokenList();
			p->atom->item = dis_new_dis_Token(strlen("NOT-CLOSED")+1);
			strcpy(p->atom->item, "NOT-CLOSED");
			p->atom->next = dis_new_dis_TokenList();
			p->atom->next->item = dis_new_dis_Token(strlen(tl->name)+1);
			strcpy(p->atom->next->item, tl->name);
			p->next = dis_gorig_initial_facts->sons;
			dis_gorig_initial_facts->sons = p; 
		}
	p = dis_new_dis_PlNode(dis_ATOM);
	p->atom = dis_new_dis_TokenList();
	p->atom->item = dis_new_dis_Token(strlen("NOT-CLOSED")+1);
	strcpy(p->atom->item, "NOT-CLOSED");
	p->atom->next = dis_new_dis_TokenList();
	p->atom->next->item = dis_new_dis_Token(strlen("EARTH")+1);
	strcpy(p->atom->next->item, "EARTH");
	p->next = dis_gorig_initial_facts->sons;
	dis_gorig_initial_facts->sons = p; 

	/* goal */
	for (p=dis_gorig_goal_facts->sons;p;p=p->next)
		if (p->connective == dis_ALL && 
		strcasecmp(p->sons->sons->atom->item, "AFFECTED") == 0)
		{
			pl = dis_new_dis_PlNode(dis_ATOM);
			pl->atom = dis_new_dis_TokenList();
			pl->atom->item = dis_new_dis_Token(strlen("NOT-AFFECTED")+1);
			strcpy(pl->atom->item, "NOT-AFFECTED");
			pl->atom->next = dis_new_dis_TokenList();
			pl->atom->next->item = dis_new_dis_Token(strlen(p->sons->sons->atom->next->item)+1);
			strcpy(pl->atom->next->item, p->sons->sons->atom->next->item);
			p->sons = pl;
			break;
		}
}
