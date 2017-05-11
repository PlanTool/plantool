/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     LEFTPAR = 258,
     RIGHTPAR = 259,
     NAME = 260,
     TWODOTS = 261,
     HYPHEN = 262,
     EITHER = 263,
     DEFINE = 264,
     DOMAIN = 265,
     REQUIREMENTS = 266,
     CONSTANTS = 267,
     PREDICATES = 268,
     QUESTION = 269,
     STRIPS = 270,
     EQUALITY = 271,
     CONDITIONAL = 272,
     DOM_AXIOMS = 273,
     DISJUNCTIVE = 274,
     ADL = 275,
     E_PRECONDITIONS = 276,
     U_PRECONDITIONS = 277,
     AND = 278,
     NOT = 279,
     FORALL = 280,
     WHEN = 281,
     EQUAL = 282,
     ACTION = 283,
     PARAMETERS = 284,
     PRECONDITION = 285,
     EFFECT = 286,
     PROBLEM = 287,
     INIT = 288,
     GOAL = 289,
     LENGTH = 290,
     SITUATION = 291,
     OBJECTS = 292,
     SERIAL = 293,
     PARALLEL = 294,
     INTEGER = 295,
     TYPING = 296,
     TYPES = 297
   };
#endif
/* Tokens.  */
#define LEFTPAR 258
#define RIGHTPAR 259
#define NAME 260
#define TWODOTS 261
#define HYPHEN 262
#define EITHER 263
#define DEFINE 264
#define DOMAIN 265
#define REQUIREMENTS 266
#define CONSTANTS 267
#define PREDICATES 268
#define QUESTION 269
#define STRIPS 270
#define EQUALITY 271
#define CONDITIONAL 272
#define DOM_AXIOMS 273
#define DISJUNCTIVE 274
#define ADL 275
#define E_PRECONDITIONS 276
#define U_PRECONDITIONS 277
#define AND 278
#define NOT 279
#define FORALL 280
#define WHEN 281
#define EQUAL 282
#define ACTION 283
#define PARAMETERS 284
#define PRECONDITION 285
#define EFFECT 286
#define PROBLEM 287
#define INIT 288
#define GOAL 289
#define LENGTH 290
#define SITUATION 291
#define OBJECTS 292
#define SERIAL 293
#define PARALLEL 294
#define INTEGER 295
#define TYPING 296
#define TYPES 297




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

