/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

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

#ifndef YY_YY_SQLGRAMMAR_TAB_H_INCLUDED
# define YY_YY_SQLGRAMMAR_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    NAME = 258,
    STRING = 259,
    INTNUM = 260,
    APPROXNUM = 261,
    OR = 262,
    AND = 263,
    NOT = 264,
    EQUALS = 265,
    GREATER_THAN = 266,
    GREATER_THAN_EQ = 267,
    LESS_THAN = 268,
    LESS_THAN_EQ = 269,
    NOT_EQUALS = 270,
    UMINUS = 271,
    ALL = 272,
    AMMSC = 273,
    ANY = 274,
    AS = 275,
    ASC = 276,
    AUTHORIZATION = 277,
    BETWEEN = 278,
    BY = 279,
    CHARACTER = 280,
    CHECK = 281,
    CLOSE = 282,
    COMMIT = 283,
    CREATE = 284,
    CURRENT = 285,
    CURSOR = 286,
    DATE_LITERAL = 287,
    DECIMAL2 = 288,
    DECLARE = 289,
    DEFAULT = 290,
    DELETE2 = 291,
    DESC = 292,
    DISTINCT = 293,
    DOUBLE2 = 294,
    DROP = 295,
    ESCAPE = 296,
    EXISTS = 297,
    FETCH = 298,
    FLOAT2 = 299,
    FOR = 300,
    FOREIGN = 301,
    FROM = 302,
    GRANT = 303,
    GROUP = 304,
    HAVING = 305,
    IN2 = 306,
    INDICATOR = 307,
    INNER = 308,
    INSERT = 309,
    INTEGER = 310,
    INTO = 311,
    IS = 312,
    JOIN = 313,
    KEY = 314,
    LANGUAGE = 315,
    LEFT = 316,
    LIKE = 317,
    MODULE = 318,
    NULLX = 319,
    NUMERIC = 320,
    OF = 321,
    ON = 322,
    OPEN = 323,
    OPTION = 324,
    ORDER = 325,
    OUTER = 326,
    PRECISION = 327,
    PRIMARY = 328,
    PRIVILEGES = 329,
    PROCEDURE = 330,
    PUBLIC = 331,
    REAL = 332,
    REFERENCES = 333,
    RIGHT = 334,
    ROLLBACK = 335,
    SCHEMA = 336,
    SELECT = 337,
    SET = 338,
    SMALLINT = 339,
    SOME = 340,
    SQLCODE = 341,
    SYSDATE = 342,
    TABLE = 343,
    TIME_LITERAL = 344,
    TO = 345,
    TRIGGER = 346,
    UNION = 347,
    UNIQUE = 348,
    UPDATE = 349,
    USER = 350,
    VALUES = 351,
    VIEW = 352,
    WHERE = 353,
    WITH = 354,
    WORK = 355,
    COBOL = 356,
    FORTRAN = 357,
    PASCAL2 = 358,
    PLI = 359,
    C = 360,
    ADA = 361,
    VARCHAR = 362
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void *root);

#endif /* !YY_YY_SQLGRAMMAR_TAB_H_INCLUDED  */
