/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison implementation for Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 3 "sqlgrammar.y" /* yacc.c:339  */



#include <stdio.h>
#include <stdlib.h>
#include "sqlstructs.h"

#define YYERROR_VERBOSE
#define YYPARSE_PARAM root
#define YYDEBUG 1

#pragma warning (disable:4013)

int block_count = 0;

typedef struct __allocated_block_struct
{
	struct __allocated_block_struct *pnext;
	char pdata[1];
} __allocated_block;

static __allocated_block *p_allocations = NULL;

void *__sql_alloc(size_t size)
{
	__allocated_block *pblock = calloc(1, size + sizeof(void*));
	pblock->pnext = p_allocations;
	p_allocations = pblock;

	block_count += 1;
	return pblock->pdata;
}

void __free_allocated_blocks()
{
	while (p_allocations)
	{
		__allocated_block *ptmp = p_allocations->pnext;
		free(p_allocations);
		block_count -= 1;
		p_allocations = ptmp;
	}
}

int yywrap(void)
{
	return 1;
}

int yyerror(void *root, char const *msg)
{
	return 1;
}

#line 121 "sqlgrammar.tab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "sqlgrammar.tab.h".  */
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

/* Copy the second part of user declarations.  */

#line 280 "sqlgrammar.tab.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  69
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   715

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  118
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  109
/* YYNRULES -- Number of rules.  */
#define YYNRULES  281
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  503

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   362

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     113,   114,    18,    16,   115,    17,   116,    19,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   117,   112,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    44,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    58,
      59,    60,    61,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,    94,    95,    96,    97,    98,
      99,   100,   101,   102,   103,   104,   105,   106,   107,   108,
     109,   110,   111
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    98,    98,   105,   110,   115,   124,   132,   142,   151,
     153,   160,   165,   173,   180,   187,   194,   202,   212,   219,
     226,   236,   241,   249,   256,   266,   278,   281,   289,   296,
     303,   310,   319,   328,   337,   344,   352,   364,   371,   378,
     389,   401,   411,   416,   424,   437,   440,   448,   451,   458,
     471,   474,   481,   488,   495,   505,   510,   518,   523,   528,
     533,   538,   548,   553,   561,   565,   572,   579,   586,   588,
     592,   593,   594,   595,   596,   597,   600,   602,   606,   607,
     611,   616,   619,   627,   632,   641,   649,   661,   664,   668,
     675,   676,   680,   685,   690,   698,   699,   703,   704,   709,
     719,   726,   733,   740,   747,   748,   749,   750,   751,   752,
     753,   757,   761,   765,   767,   771,   779,   789,   793,   804,
     811,   821,   826,   834,   838,   845,   849,   853,   868,   871,
     875,   882,   886,   887,   892,   900,   907,   917,   928,   933,
     941,   949,   952,   960,   970,   976,   983,   993,  1000,  1011,
    1023,  1030,  1045,  1052,  1065,  1071,  1077,  1083,  1090,  1098,
    1106,  1118,  1123,  1131,  1138,  1146,  1154,  1165,  1173,  1176,
    1183,  1188,  1197,  1200,  1209,  1217,  1225,  1232,  1239,  1248,
    1255,  1262,  1269,  1276,  1283,  1290,  1300,  1309,  1321,  1330,
    1342,  1351,  1364,  1367,  1374,  1381,  1391,  1400,  1409,  1418,
    1430,  1435,  1443,  1455,  1459,  1463,  1470,  1477,  1486,  1496,
    1506,  1514,  1522,  1530,  1538,  1546,  1554,  1560,  1566,  1572,
    1581,  1586,  1594,  1601,  1608,  1618,  1619,  1620,  1624,  1631,
    1639,  1646,  1654,  1661,  1670,  1674,  1681,  1688,  1695,  1702,
    1709,  1720,  1728,  1740,  1746,  1756,  1762,  1772,  1778,  1788,
    1798,  1805,  1816,  1827,  1838,  1839,  1840,  1841,  1842,  1843,
    1847,  1854,  1861,  1867,  1874,  1882,  1888,  1895,  1903,  1909,
    1915,  1921,  1927,  1933,  1940,  1951,  1957,  1963,  1970,  1976,
    1982,  1988
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "NAME", "STRING", "INTNUM", "APPROXNUM",
  "OR", "AND", "NOT", "EQUALS", "GREATER_THAN", "GREATER_THAN_EQ",
  "LESS_THAN", "LESS_THAN_EQ", "NOT_EQUALS", "'+'", "'-'", "'*'", "'/'",
  "UMINUS", "ALL", "AMMSC", "ANY", "AS", "ASC", "AUTHORIZATION", "BETWEEN",
  "BY", "CHARACTER", "CHECK", "CLOSE", "COMMIT", "CREATE", "CURRENT",
  "CURSOR", "DATE_LITERAL", "DECIMAL2", "DECLARE", "DEFAULT", "DELETE2",
  "DESC", "DISTINCT", "DOUBLE2", "DROP", "ESCAPE", "EXISTS", "FETCH",
  "FLOAT2", "FOR", "FOREIGN", "FROM", "GRANT", "GROUP", "HAVING", "IN2",
  "INDICATOR", "INNER", "INSERT", "INTEGER", "INTO", "IS", "JOIN", "KEY",
  "LANGUAGE", "LEFT", "LIKE", "MODULE", "NULLX", "NUMERIC", "OF", "ON",
  "OPEN", "OPTION", "ORDER", "OUTER", "PRECISION", "PRIMARY", "PRIVILEGES",
  "PROCEDURE", "PUBLIC", "REAL", "REFERENCES", "RIGHT", "ROLLBACK",
  "SCHEMA", "SELECT", "SET", "SMALLINT", "SOME", "SQLCODE", "SYSDATE",
  "TABLE", "TIME_LITERAL", "TO", "TRIGGER", "UNION", "UNIQUE", "UPDATE",
  "USER", "VALUES", "VIEW", "WHERE", "WITH", "WORK", "COBOL", "FORTRAN",
  "PASCAL2", "PLI", "C", "ADA", "VARCHAR", "';'", "'('", "')'", "','",
  "'.'", "':'", "$accept", "root", "sql_list", "sql", "schema",
  "opt_schema_element_list", "schema_element_list", "schema_element",
  "drop_trigger", "drop_table", "base_table_def",
  "base_table_element_commalist", "base_table_element", "column_def",
  "column_def_opt_list", "column_def_opt", "table_constraint_def",
  "column_commalist", "view_def", "opt_with_check_option",
  "opt_column_commalist", "privilege_def", "opt_with_grant_option",
  "privileges", "operation_commalist", "operation", "grantee_commalist",
  "grantee", "module_def", "opt_module", "lang", "opt_cursor_def_list",
  "cursor_def_list", "cursor_def", "opt_order_by_clause",
  "ordering_spec_commalist", "ordering_spec", "opt_asc_desc",
  "procedure_def_list", "procedure_def", "manipulative_statement_list",
  "parameter_def_list", "parameter_def", "manipulative_statement",
  "close_statement", "commit_statement", "delete_statement_positioned",
  "delete_statement_searched", "fetch_statement", "insert_statement",
  "values_or_query_spec", "insert_atom_commalist", "insert_atom",
  "open_statement", "rollback_statement", "select_statement",
  "opt_all_distinct", "update_statement_positioned",
  "assignment_commalist", "assignment", "update_statement_searched",
  "target_commalist", "target", "opt_where_clause", "query_exp",
  "query_term", "query_spec", "selection", "table_exp", "from_clause",
  "join_ref", "table_ref_commalist", "table_ref", "where_clause",
  "opt_group_by_clause", "column_ref_commalist", "opt_having_clause",
  "search_condition", "predicate", "comparison_predicate",
  "between_predicate", "like_predicate", "opt_escape", "test_for_null",
  "in_predicate", "atom_commalist", "all_or_any_predicate", "any_all_some",
  "existence_test", "subquery", "scalar_exp", "scalar_exp_commalist",
  "atom", "parameter_ref", "function_ref", "literal", "string_literal",
  "table", "trigger_name", "column_ref", "comparison", "data_type",
  "column", "cursor", "module", "parameter", "procedure", "range_variable",
  "user", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,    43,    45,    42,    47,
     271,   272,   273,   274,   275,   276,   277,   278,   279,   280,
     281,   282,   283,   284,   285,   286,   287,   288,   289,   290,
     291,   292,   293,   294,   295,   296,   297,   298,   299,   300,
     301,   302,   303,   304,   305,   306,   307,   308,   309,   310,
     311,   312,   313,   314,   315,   316,   317,   318,   319,   320,
     321,   322,   323,   324,   325,   326,   327,   328,   329,   330,
     331,   332,   333,   334,   335,   336,   337,   338,   339,   340,
     341,   342,   343,   344,   345,   346,   347,   348,   349,   350,
     351,   352,   353,   354,   355,   356,   357,   358,   359,   360,
     361,   362,    59,    40,    41,    44,    46,    58
};
# endif

#define YYPACT_NINF -417

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-417)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     571,    33,   -34,   189,    55,   -20,    33,   180,    14,   135,
      33,    66,   154,   197,   249,   571,   140,  -417,  -417,  -417,
    -417,  -417,  -417,  -417,  -417,  -417,  -417,  -417,  -417,  -417,
    -417,  -417,  -417,  -417,  -417,  -417,  -417,  -417,  -417,  -417,
    -417,   244,   197,   197,   166,   197,   178,   197,   292,   224,
     210,  -417,  -417,   211,  -417,   211,   246,   195,  -417,   197,
    -417,   268,  -417,  -417,  -417,  -417,  -417,   538,   263,  -417,
     230,  -417,   348,   243,   211,   366,   276,   389,  -417,  -417,
    -417,   267,  -417,   293,  -417,   382,  -417,  -417,   197,   279,
     211,   527,    17,  -417,  -417,  -417,   538,   538,  -417,   278,
    -417,  -417,   538,   236,   363,   299,  -417,  -417,   400,  -417,
     382,  -417,  -417,   281,    31,   398,  -417,   412,  -417,   494,
     367,   311,   494,   127,  -417,  -417,  -417,  -417,  -417,  -417,
    -417,  -417,   649,   375,   439,   440,   330,  -417,  -417,   -10,
    -417,   226,  -417,   356,  -417,   141,  -417,  -417,  -417,  -417,
    -417,  -417,   429,  -417,   456,   190,   173,   173,   354,    35,
     197,   293,   386,   359,  -417,   538,   538,   538,   538,   469,
     538,  -417,   149,  -417,   467,   235,  -417,   281,  -417,   365,
     417,   420,   371,   239,  -417,  -417,  -417,   509,   399,   424,
    -417,    33,   399,  -417,    11,   289,   494,   494,    97,  -417,
    -417,  -417,  -417,  -417,  -417,   538,   377,   229,   470,    53,
    -417,  -417,   293,   293,  -417,  -417,   382,    24,   154,   388,
    -417,  -417,   348,  -417,    23,  -417,   381,   538,   501,  -417,
      47,   393,  -417,   394,   130,   212,   -18,   480,  -417,   494,
     460,   204,   204,   173,   173,  -417,   363,   435,   382,  -417,
     255,  -417,   494,   401,   402,   382,  -417,    31,   404,   409,
     447,  -417,  -417,   411,  -417,  -417,   413,  -417,   426,    33,
    -417,   421,  -417,   519,  -417,   538,   423,   229,   446,   112,
    -417,   418,  -417,   500,  -417,  -417,  -417,  -417,  -417,   515,
     311,  -417,   363,   471,  -417,  -417,  -417,  -417,  -417,    29,
    -417,  -417,   538,   176,   511,  -417,   544,   547,  -417,    95,
      25,   444,  -417,  -417,   197,   130,   197,   478,   486,  -417,
    -417,   559,  -417,  -417,   164,   536,   512,   495,  -417,  -417,
     363,    15,   382,   382,   248,  -417,   562,   564,  -417,   565,
     566,   144,   542,  -417,  -417,  -417,   576,   112,  -417,   500,
     538,   259,  -417,   555,   229,  -417,  -417,  -417,   523,    24,
    -417,   525,  -417,   274,  -417,  -417,    33,   498,   511,  -417,
    -417,  -417,  -417,   577,  -417,  -417,  -417,   264,   130,   130,
    -417,   177,   466,  -417,   177,   501,   494,  -417,    33,  -417,
     285,   287,  -417,   468,   294,   298,   472,   517,   475,   250,
     197,  -417,   516,   538,   305,  -417,   363,  -417,   229,  -417,
     518,  -417,  -417,   176,   561,   595,   498,  -417,  -417,   494,
    -417,  -417,  -417,  -417,  -417,  -417,   164,  -417,   484,  -417,
     127,  -417,   524,  -417,  -417,  -417,   600,  -417,   604,  -417,
     186,   494,  -417,  -417,  -417,   497,  -417,   363,  -417,  -417,
    -417,  -417,   563,  -417,   -35,  -417,   232,  -417,   501,   197,
     499,   502,   551,  -417,    22,   382,    45,  -417,    60,  -417,
     509,  -417,  -417,   504,  -417,  -417,  -417,  -417,   312,    45,
      -6,  -417,  -417,   159,  -417,  -417,   382,  -417,    90,    71,
    -417,   154,   159,  -417,   317,  -417,    45,  -417,   538,  -417,
    -417,  -417,   567
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       0,     0,     0,     0,     0,     0,     0,     0,     0,    68,
       0,     0,   128,     0,     0,     2,     5,     6,     7,    16,
      17,    13,    14,    15,    66,    99,   104,   105,   106,   100,
     107,   101,   108,   109,   102,   110,   103,   143,   276,   111,
     112,     0,     0,     0,   243,     0,   141,     0,     0,     0,
      53,    59,    58,    47,    57,    47,     0,    54,    55,     0,
     277,     0,    69,   125,   126,   129,   130,     0,     0,     1,
       0,     3,     0,     0,    47,     0,   141,     0,   116,   142,
      19,   245,    18,     0,    52,     0,    61,    60,     0,     0,
      47,     0,   247,   241,   235,   236,     0,     0,   237,     0,
     239,   240,     0,     0,   220,   150,   218,   216,   234,   217,
     132,     4,   281,     9,     0,     0,   244,     0,   115,     0,
       0,     0,     0,   167,   178,   179,   183,   184,   181,   180,
     185,   182,     0,   217,     0,     0,   117,   138,   140,   225,
     275,     0,    42,     0,    56,     0,    70,    71,    72,    73,
      74,    75,     0,   248,     0,     0,   214,   215,     0,     0,
       0,     0,    81,   141,   208,     0,     0,     0,     0,     0,
       0,   242,   141,   133,     0,     0,     8,    10,    11,     0,
       0,     0,     0,     0,    21,    23,    24,     0,     0,     0,
     176,     0,     0,   206,     0,     0,     0,     0,     0,   254,
     256,   258,   257,   259,   255,     0,     0,     0,     0,     0,
     246,   278,     0,     0,   226,    48,     0,     0,   128,     0,
     118,   120,     0,   249,   250,   238,   237,     0,     0,   233,
       0,   217,   219,   152,   161,   163,     0,     0,   149,     0,
     168,   210,   211,   212,   213,   209,   221,     0,     0,   137,
       0,    12,     0,     0,     0,     0,    20,     0,   260,   265,
       0,   270,   268,   262,   271,   269,   273,    26,    45,     0,
     114,     0,   177,   174,   175,     0,     0,     0,     0,     0,
     197,     0,   224,   192,   222,   223,   204,   203,   205,     0,
       0,   187,   186,     0,   195,   139,   227,    43,    64,    50,
      62,    65,     0,     0,    76,   251,     0,     0,   228,     0,
     247,     0,   232,   230,     0,     0,     0,     0,     0,   153,
     164,     0,   166,   127,     0,     0,   172,     0,   134,   136,
     135,     0,     0,     0,     0,    22,     0,     0,   272,     0,
       0,    25,     0,    44,   113,   207,     0,     0,   196,   192,
       0,     0,   200,     0,     0,   191,   202,   194,     0,     0,
      49,     0,   124,     0,   121,   123,     0,     0,    77,    78,
     252,   253,   231,     0,   229,   162,   154,   157,     0,     0,
     165,    87,    82,    83,    87,     0,     0,   151,     0,    41,
       0,     0,    37,     0,     0,     0,     0,     0,     0,     0,
       0,    27,     0,     0,     0,   190,   189,   199,     0,   193,
       0,    63,   119,     0,     0,     0,    67,    90,    79,     0,
     158,   155,   156,    88,    89,    86,     0,    85,   169,   170,
     173,   131,     0,    38,   261,   266,     0,   263,     0,   274,
      28,     0,    32,    33,    31,    35,    46,   188,   198,   201,
      51,   122,     0,   279,     0,    91,   159,    84,     0,     0,
       0,     0,     0,    29,     0,     0,     0,    98,     0,    95,
       0,   160,   171,    39,   267,   264,    30,    34,     0,     0,
      81,   144,   147,     0,    96,    97,     0,    36,     0,     0,
      80,   128,    92,    93,     0,   148,     0,   145,     0,    94,
      40,   146,     0
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -417,  -417,  -417,   606,  -417,  -417,  -417,   -78,  -417,  -417,
    -417,  -417,   362,  -417,  -417,  -417,  -417,  -244,  -417,  -417,
      36,  -417,  -417,  -417,  -417,   533,  -417,   265,  -417,  -417,
    -417,  -417,  -417,   257,   146,  -417,   213,   256,  -417,   214,
    -417,  -417,   174,  -153,  -417,  -417,  -417,  -417,  -417,  -417,
    -417,  -417,   228,  -417,  -417,  -417,  -212,  -417,  -417,   396,
    -417,   485,   433,   -16,   168,  -416,     0,  -295,   414,  -417,
    -301,  -417,   -68,  -417,  -417,  -417,  -417,  -110,  -417,  -417,
    -417,  -417,   300,  -417,  -417,   301,  -417,  -417,  -417,  -139,
     -65,  -417,  -194,   -66,  -417,  -183,  -417,    -3,  -417,   -74,
    -417,   183,   -89,    -2,  -417,  -134,  -417,  -417,   -56
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    14,    15,    16,    17,   176,   177,    18,    19,    20,
      21,   183,   184,   185,   341,   401,   186,   141,    22,   343,
      86,    23,   360,    56,    57,    58,   299,   300,    24,    61,
     152,   367,   368,   369,   238,   382,   383,   425,   416,   417,
     492,   468,   469,    25,    26,    27,    28,    29,    30,    31,
     220,   363,   364,    32,    33,    34,    67,    35,   172,   173,
      36,   136,   137,    78,   480,   481,   271,   103,   162,   163,
     319,   233,   234,    79,   326,   428,   387,   123,   124,   125,
     126,   127,   355,   128,   129,   351,   130,   290,   131,   193,
     132,   105,   352,   284,   106,   107,   108,   235,    82,   109,
     208,   267,   142,    39,    62,   139,   454,   322,   301
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint16 yytable[] =
{
      37,    46,   104,   133,    49,   214,   302,   361,    63,   190,
      68,   334,   194,   283,   376,    37,   113,   138,   196,   197,
     153,   174,   196,   197,   285,   187,   305,   112,   153,   196,
     197,   156,   157,   160,   140,   178,    38,   159,   164,    73,
      74,   154,    76,   133,    80,   133,   213,   306,   133,   154,
     164,   165,   166,   167,   168,   467,    90,   195,    44,   169,
     118,   179,   293,   165,   166,   167,   168,   280,   237,   291,
      40,   169,    47,   497,    59,    48,   420,   421,   422,   296,
     501,   180,   135,   349,   231,   143,   273,   274,   390,   391,
     489,    87,   496,   230,   285,   138,   285,   212,   164,   251,
     241,   242,   243,   244,   298,   246,    45,   135,   181,   365,
     115,   165,   166,   167,   168,   281,    93,    94,    95,   169,
     285,   294,   133,   133,   275,   272,   145,   297,   182,   389,
      98,   218,   358,   155,   196,   197,   477,   348,    60,   307,
     278,   373,   331,   292,   359,   221,   138,   240,   100,   232,
     467,   356,   276,   397,   311,   471,   249,   218,   479,   174,
     409,   312,   309,   277,   285,   133,   304,   310,   187,   381,
      64,   285,   483,   133,   398,    65,   164,   135,   133,   281,
      93,    94,    95,   399,   479,   330,   489,   315,   268,   270,
       1,     2,   316,   224,    98,   317,    66,   169,   218,     4,
      44,    50,   423,   502,   495,   101,     6,   164,   225,   372,
     346,   282,   100,   318,   449,   320,   444,     8,   424,   365,
      51,   478,   167,   168,   159,   285,   400,   218,   169,   135,
     285,    10,   281,    93,    94,    95,   321,   104,    52,   196,
     197,   219,   494,    11,   362,   491,   375,    98,   377,    69,
     384,   247,    71,   281,    93,    94,    95,    13,    92,    93,
      94,    95,    53,   462,   248,   100,    54,   344,    98,   101,
      72,    96,    97,    98,    41,   282,   430,    99,    55,   498,
      77,    42,    75,   463,    83,   406,   100,   160,    84,   315,
      43,   100,   164,   135,   316,    81,   161,   317,   198,   199,
     200,   201,   202,   203,   204,   165,   166,   167,   168,   456,
      89,   429,   133,   169,   175,   318,   205,    88,   442,    51,
     470,   315,   101,   329,    85,     5,   316,    42,   282,   317,
     493,   464,    91,     7,   470,   419,    43,    52,   447,   499,
     215,   216,   111,   101,   206,   133,   135,   318,   101,   443,
     110,   112,   384,   256,   257,   207,   114,    92,    93,    94,
      95,    53,   392,   216,   414,    54,   164,   133,   102,   116,
      96,    97,   226,   407,   408,   227,    99,    55,   117,   165,
     166,   167,   168,   134,   472,   140,   431,   169,   412,   413,
     100,   158,    92,    93,    94,    95,   228,   445,   119,   432,
     216,   433,   216,   232,   171,    96,    97,    98,   435,   436,
     135,    99,   437,   438,   170,    92,    93,    94,    95,   448,
     408,   119,   188,   120,   192,   100,   487,   216,    96,    97,
      98,   500,   216,   104,    99,   121,   209,   191,    92,    93,
      94,    95,   210,   211,   119,   212,   189,   101,   100,   164,
     217,    96,    97,    98,   350,   222,   473,    99,   121,   223,
     237,   239,   165,   166,   167,   168,   482,   102,   229,   327,
     169,   100,   245,    92,    93,    94,    95,   250,   252,   482,
     253,   121,   101,   254,   255,   218,    96,    97,    98,   482,
     279,   286,    99,   287,   269,   308,   482,    92,    93,    94,
      95,   303,   122,   119,   310,   101,   100,   313,   324,   314,
      96,    97,    98,   325,   332,   333,    99,   336,    92,    93,
      94,    95,   337,   338,   339,   122,   340,   197,   101,   342,
     100,    96,    97,    98,   353,   345,   347,    99,   258,   357,
     121,    92,    93,    94,    95,   354,   259,   370,   122,   366,
     371,   100,   260,   378,    96,    97,    98,   261,   374,   288,
      99,   379,   380,   101,   385,   388,   386,   393,   262,   394,
     395,   396,   402,   225,   100,   410,   160,   415,   263,   164,
     224,   426,   434,   289,   403,   440,   439,   101,   441,   446,
     264,   450,   165,   166,   167,   168,   452,   265,   453,   458,
     169,   218,     1,     2,     3,   460,   459,   122,   101,   461,
     465,     4,   466,   474,   476,     5,   475,   486,     6,   335,
     266,    70,   144,     7,   411,   418,   490,   161,   102,     8,
     455,   101,   146,   147,   148,   149,   150,   151,     9,   457,
     427,   451,   484,    10,   328,   295,   236,   488,   404,   405,
     323,   102,   164,   485,     0,    11,     0,    12,   198,   199,
     200,   201,   202,   203,   204,   165,   166,   167,   168,    13,
       0,     0,     0,   169,     0,     0,   205,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   206,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   207
};

static const yytype_int16 yycheck[] =
{
       0,     4,    67,    77,     6,   139,   218,   302,    10,   119,
      13,   255,   122,   207,   315,    15,    72,    83,     7,     8,
       3,   110,     7,     8,   207,   114,     3,     3,     3,     7,
       8,    96,    97,    51,     3,   113,     3,   102,     3,    42,
      43,    24,    45,   117,    47,   119,    56,    24,   122,    24,
       3,    16,    17,    18,    19,    90,    59,   122,     3,    24,
      76,    30,     9,    16,    17,    18,    19,   206,    74,   208,
     104,    24,    92,   489,    60,    95,   377,   378,   379,   213,
     496,    50,   117,   277,   158,    88,   196,   197,   332,   333,
      96,    55,    21,   158,   277,   161,   279,   115,     3,   177,
     165,   166,   167,   168,    80,   170,    51,   117,    77,   303,
      74,    16,    17,    18,    19,     3,     4,     5,     6,    24,
     303,    68,   196,   197,    27,   114,    90,   216,    97,   114,
      18,    86,   103,   116,     7,     8,   114,   276,     3,   116,
     205,   116,   252,   208,   115,   145,   212,   163,    36,   114,
      90,   290,    55,     9,   228,   456,   172,    86,   113,   248,
     354,   114,   227,    66,   347,   239,   222,     3,   257,     5,
     104,   354,   112,   247,    30,    21,     3,   117,   252,     3,
       4,     5,     6,    39,   113,   250,    96,    57,   188,   191,
      31,    32,    62,     3,    18,    65,    42,    24,    86,    40,
       3,    21,    25,   498,   114,    93,    47,     3,    18,   114,
     275,    99,    36,    83,   408,     3,   399,    58,    41,   413,
      40,   465,    18,    19,   289,   408,    82,    86,    24,   117,
     413,    72,     3,     4,     5,     6,    24,   302,    58,     7,
       8,   100,   486,    84,    68,    86,   314,    18,   316,     0,
     324,   102,   112,     3,     4,     5,     6,    98,     3,     4,
       5,     6,    82,    77,   115,    36,    86,   269,    18,    93,
      26,    16,    17,    18,    85,    99,   386,    22,    98,   491,
     102,    92,   116,    97,    60,   350,    36,    51,    78,    57,
     101,    36,     3,   117,    62,     3,    60,    65,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,   419,
     115,   385,   386,    24,    33,    83,    27,    71,    68,    40,
     454,    57,    93,    68,   113,    44,    62,    92,    99,    65,
     483,   441,    64,    52,   468,    71,   101,    58,   403,   492,
     114,   115,   112,    93,    55,   419,   117,    83,    93,    99,
      87,     3,   426,   114,   115,    66,   113,     3,     4,     5,
       6,    82,   114,   115,   366,    86,     3,   441,   113,     3,
      16,    17,    18,   114,   115,    21,    22,    98,   102,    16,
      17,    18,    19,   116,   458,     3,   388,    24,   114,   115,
      36,   113,     3,     4,     5,     6,    42,   400,     9,   114,
     115,   114,   115,   114,     4,    16,    17,    18,   114,   115,
     117,    22,   114,   115,   115,     3,     4,     5,     6,   114,
     115,     9,    24,    34,   113,    36,   114,   115,    16,    17,
      18,   114,   115,   498,    22,    46,    61,    70,     3,     4,
       5,     6,     3,     3,     9,   115,    34,    93,    36,     3,
      94,    16,    17,    18,     8,    26,   459,    22,    46,     3,
      74,   102,    16,    17,    18,    19,   466,   113,   114,    34,
      24,    36,     3,     3,     4,     5,     6,    10,   113,   479,
      63,    46,    93,    63,   113,    86,    16,    17,    18,   489,
     113,    21,    22,    23,    70,   114,   496,     3,     4,     5,
       6,   113,   113,     9,     3,    93,    36,   114,    28,   115,
      16,    17,    18,    53,   113,   113,    22,   113,     3,     4,
       5,     6,   113,    76,   113,   113,   113,     8,    93,   103,
      36,    16,    17,    18,   116,   114,   113,    22,    29,    68,
      46,     3,     4,     5,     6,    45,    37,     3,   113,    38,
       3,    36,    43,    75,    16,    17,    18,    48,   114,    89,
      22,    75,     3,    93,    28,    70,    54,     5,    59,     5,
       5,     5,    30,    18,    36,    52,    51,    79,    69,     3,
       3,   115,   114,   113,     8,    68,   114,    93,   113,    73,
      81,    73,    16,    17,    18,    19,    35,    88,     3,   115,
      24,    86,    31,    32,    33,     5,    82,   113,    93,     5,
     113,    40,    49,   114,    63,    44,   114,   113,    47,   257,
     111,    15,    89,    52,   359,   368,   480,    60,   113,    58,
     416,    93,   105,   106,   107,   108,   109,   110,    67,   426,
     384,   413,   468,    72,   248,   212,   161,   479,   347,   349,
     236,   113,     3,   470,    -1,    84,    -1,    86,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    98,
      -1,    -1,    -1,    24,    -1,    -1,    27,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    55,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    66
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    31,    32,    33,    40,    44,    47,    52,    58,    67,
      72,    84,    86,    98,   119,   120,   121,   122,   125,   126,
     127,   128,   136,   139,   146,   161,   162,   163,   164,   165,
     166,   167,   171,   172,   173,   175,   178,   184,     3,   221,
     104,    85,    92,   101,     3,    51,   215,    92,    95,   221,
      21,    40,    58,    82,    86,    98,   141,   142,   143,    60,
       3,   147,   222,   221,   104,    21,    42,   174,   215,     0,
     121,   112,    26,   215,   215,   116,   215,   102,   181,   191,
     215,     3,   216,    60,    78,   113,   138,   138,    71,   115,
     215,    64,     3,     4,     5,     6,    16,    17,    18,    22,
      36,    93,   113,   185,   208,   209,   212,   213,   214,   217,
      87,   112,     3,   226,   113,   138,     3,   102,   181,     9,
      34,    46,   113,   195,   196,   197,   198,   199,   201,   202,
     204,   206,   208,   217,   116,   117,   179,   180,   211,   223,
       3,   135,   220,   215,   143,   138,   105,   106,   107,   108,
     109,   110,   148,     3,    24,   116,   208,   208,   113,   208,
      51,    60,   186,   187,     3,    16,    17,    18,    19,    24,
     115,     4,   176,   177,   220,    33,   123,   124,   125,    30,
      50,    77,    97,   129,   130,   131,   134,   220,    24,    34,
     195,    70,   113,   207,   195,   208,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    27,    55,    66,   218,    61,
       3,     3,   115,    56,   223,   114,   115,    94,    86,   100,
     168,   184,    26,     3,     3,    18,    18,    21,    42,   114,
     208,   217,   114,   189,   190,   215,   179,    74,   152,   102,
     181,   208,   208,   208,   208,     3,   208,   102,   115,   181,
      10,   125,   113,    63,    63,   113,   114,   115,    29,    37,
      43,    48,    59,    69,    81,    88,   111,   219,   184,    70,
     221,   184,   114,   195,   195,    27,    55,    66,   208,   113,
     207,     3,    99,   210,   211,   213,    21,    23,    89,   113,
     205,   207,   208,     9,    68,   180,   223,   220,    80,   144,
     145,   226,   174,   113,   226,     3,    24,   116,   114,   208,
       3,   217,   114,   114,   115,    57,    62,    65,    83,   188,
       3,    24,   225,   186,    28,    53,   192,    34,   177,    68,
     208,   195,   113,   113,   135,   130,   113,   113,    76,   113,
     113,   132,   103,   137,   221,   114,   208,   113,   207,   210,
       8,   203,   210,   116,    45,   200,   207,    68,   103,   115,
     140,   185,    68,   169,   170,   210,    38,   149,   150,   151,
       3,     3,   114,   116,   114,   190,   188,   190,    75,    75,
       3,     5,   153,   154,   217,    28,    54,   194,    70,   114,
     135,   135,   114,     5,     5,     5,     5,     9,    30,    39,
      82,   133,    30,     8,   203,   200,   208,   114,   115,   210,
      52,   145,   114,   115,   221,    79,   156,   157,   151,    71,
     188,   188,   188,    25,    41,   155,   115,   155,   193,   217,
     195,   221,   114,   114,   114,   114,   115,   114,   115,   114,
      68,   113,    68,    99,   213,   215,    73,   208,   114,   210,
      73,   170,    35,     3,   224,   157,   195,   154,   115,    82,
       5,     5,    77,    97,   195,   113,    49,    90,   159,   160,
     223,   188,   217,   215,   114,   114,    63,   114,   135,   113,
     182,   183,   184,   112,   160,   219,   113,   114,   182,    96,
     152,    86,   158,   161,   135,   114,    21,   183,   174,   161,
     114,   183,   185
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,   118,   119,   120,   120,   120,   121,   121,   122,   123,
     123,   124,   124,   125,   125,   125,   125,   125,   126,   127,
     128,   129,   129,   130,   130,   131,   132,   132,   133,   133,
     133,   133,   133,   133,   133,   133,   133,   134,   134,   134,
     134,   134,   135,   135,   136,   137,   137,   138,   138,   139,
     140,   140,   141,   141,   141,   142,   142,   143,   143,   143,
     143,   143,   144,   144,   145,   145,   121,   146,   147,   147,
     148,   148,   148,   148,   148,   148,   149,   149,   150,   150,
     151,   152,   152,   153,   153,   154,   154,   155,   155,   155,
     156,   156,   157,   158,   158,   159,   159,   160,   160,   121,
     161,   161,   161,   161,   161,   161,   161,   161,   161,   161,
     161,   162,   163,   164,   164,   165,   165,   166,   167,   168,
     168,   169,   169,   170,   170,   171,   172,   173,   174,   174,
     174,   175,   176,   176,   176,   177,   177,   178,   179,   179,
     180,   181,   181,   121,   182,   182,   182,   183,   183,   184,
     185,   186,   187,   187,   188,   188,   188,   188,   188,   188,
     188,   189,   189,   190,   190,   190,   190,   191,   192,   192,
     193,   193,   194,   194,   195,   195,   195,   195,   195,   196,
     196,   196,   196,   196,   196,   196,   197,   197,   198,   198,
     199,   199,   200,   200,   201,   201,   202,   202,   202,   202,
     203,   203,   204,   205,   205,   205,   206,   207,   208,   208,
     208,   208,   208,   208,   208,   208,   208,   208,   208,   208,
     209,   209,   210,   210,   210,   211,   211,   211,   212,   212,
     212,   212,   212,   212,   213,   213,   213,   213,   213,   213,
     213,   214,   214,   215,   215,   216,   216,   217,   217,   217,
     217,   217,   217,   217,   218,   218,   218,   218,   218,   218,
     219,   219,   219,   219,   219,   219,   219,   219,   219,   219,
     219,   219,   219,   219,   219,   220,   221,   222,   223,   224,
     225,   226
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     3,     1,     1,     1,     5,     0,
       1,     1,     2,     1,     1,     1,     1,     1,     3,     3,
       6,     1,     3,     1,     1,     3,     0,     2,     2,     3,
       4,     2,     2,     2,     4,     2,     5,     4,     5,     7,
      10,     4,     1,     3,     7,     0,     3,     0,     3,     7,
       0,     3,     2,     1,     1,     1,     3,     1,     1,     1,
       2,     2,     1,     3,     1,     1,     1,     8,     0,     1,
       1,     1,     1,     1,     1,     1,     0,     1,     1,     2,
       6,     0,     3,     1,     3,     2,     2,     0,     1,     1,
       1,     2,     5,     1,     2,     1,     2,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     2,     7,     6,     4,     3,     4,     5,     4,
       1,     1,     3,     1,     1,     2,     2,     6,     0,     1,
       1,     8,     0,     1,     3,     3,     3,     5,     1,     3,
       1,     0,     1,     1,     1,     3,     4,     1,     3,     5,
       1,     4,     2,     3,     2,     3,     3,     2,     3,     4,
       5,     1,     3,     1,     2,     3,     2,     2,     0,     3,
       1,     3,     0,     2,     3,     3,     2,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     3,     6,     5,
       5,     4,     0,     2,     4,     3,     4,     3,     6,     5,
       1,     3,     4,     1,     1,     1,     2,     3,     2,     3,
       3,     3,     3,     3,     2,     2,     1,     1,     1,     3,
       1,     3,     1,     1,     1,     1,     2,     3,     4,     5,
       4,     5,     4,     3,     1,     1,     1,     1,     3,     1,
       1,     1,     2,     1,     3,     1,     3,     1,     2,     3,
       3,     4,     5,     5,     1,     1,     1,     1,     1,     1,
       1,     4,     1,     4,     6,     1,     4,     6,     1,     1,
       1,     1,     2,     1,     4,     1,     1,     1,     2,     1,
       1,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (root, YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value, root); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, void *root)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  YYUSE (root);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, void *root)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep, root);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule, void *root)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              , root);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule, root); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, void *root)
{
  YYUSE (yyvaluep);
  YYUSE (root);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void *root)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 99 "sqlgrammar.y" /* yacc.c:1646  */
    {
			*((sql_list_t**)root) = (sql_list_t*)(yyvsp[0]);
		}
#line 1812 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 3:
#line 106 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = _sql_create_list();
			_sql_add_tail((yyval), (yyvsp[-1]));
		}
#line 1821 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 4:
#line 111 "sqlgrammar.y" /* yacc.c:1646  */
    {
			_sql_add_tail((yyvsp[-2]), (yyvsp[-1])); 
			(yyval) = (yyvsp[-2]);
		}
#line 1830 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 5:
#line 116 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = _sql_create_list();
			_sql_add_tail((yyval), (yyvsp[0]));
		}
#line 1839 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 6:
#line 125 "sqlgrammar.y" /* yacc.c:1646  */
    {
				sql_declare(sql_t, psql);
				psql->which = 0;
				psql->pschema = (schema_t*)(yyvsp[0]);
				(yyval) = (LITEM)psql;
			}
#line 1850 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 7:
#line 133 "sqlgrammar.y" /* yacc.c:1646  */
    {
				sql_declare(sql_t, psql);
				psql->which = 4;
				psql->pschema_element = (schema_element_t*)(yyvsp[0]);
				(yyval) = (LITEM)psql;
			}
#line 1861 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 8:
#line 143 "sqlgrammar.y" /* yacc.c:1646  */
    {
			schema_t *pschema = sql_alloc(schema_t);
			pschema->puser = (user_t*)(yyvsp[-1]);
			pschema->plist = (yyvsp[0]);
			(yyval) = (LITEM)pschema;
		}
#line 1872 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 10:
#line 154 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (yyvsp[0]);
		}
#line 1880 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 11:
#line 161 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = _sql_create_list();
			_sql_add_tail((yyval), (yyvsp[0]));
		}
#line 1889 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 12:
#line 166 "sqlgrammar.y" /* yacc.c:1646  */
    {
			_sql_add_tail((yyvsp[-1]), (yyvsp[0])); 
			(yyval) = (yyvsp[-1]);
		}
#line 1898 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 13:
#line 174 "sqlgrammar.y" /* yacc.c:1646  */
    {
			schema_element_t *pelement = sql_alloc(schema_element_t);
			pelement->which = 0;
			pelement->ptabledef = (base_table_def_t *)(yyvsp[0]);
			(yyval) = (LITEM)pelement;
		}
#line 1909 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 14:
#line 181 "sqlgrammar.y" /* yacc.c:1646  */
    {
			schema_element_t *pelement = sql_alloc(schema_element_t);
			pelement->which = 1;
			pelement->pviewdef = (view_def_t *)(yyvsp[0]);
			(yyval) = (LITEM)pelement;
		}
#line 1920 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 15:
#line 188 "sqlgrammar.y" /* yacc.c:1646  */
    {
			schema_element_t *pelement = sql_alloc(schema_element_t);
			pelement->which = 2;
			pelement->pprivilegedef = (privilege_def_t *)(yyvsp[0]);
			(yyval) = (LITEM)pelement;
		}
#line 1931 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 195 "sqlgrammar.y" /* yacc.c:1646  */
    {
			schema_element_t *pelement = sql_alloc(schema_element_t);
			pelement->which = 3;
			pelement->pdroptrigger = (drop_trigger_t*)(yyvsp[0]);
			(yyval) = (LITEM)pelement;
		}
#line 1942 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 17:
#line 203 "sqlgrammar.y" /* yacc.c:1646  */
    {
			schema_element_t *pelement = sql_alloc(schema_element_t);
			pelement->which = 4;
			pelement->pdroptable = (drop_table_t*)(yyvsp[0]);
			(yyval) = (LITEM)pelement;
		}
#line 1953 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 213 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (yyvsp[0]);
		}
#line 1961 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 19:
#line 220 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (yyvsp[0]);
		}
#line 1969 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 20:
#line 227 "sqlgrammar.y" /* yacc.c:1646  */
    {
			base_table_def_t *pdef = sql_alloc(base_table_def_t);
			pdef->ptable = (table_t*)(yyvsp[-3]);
			pdef->plist = (yyvsp[-1]);
			(yyval) = (LITEM)pdef;
		}
#line 1980 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 21:
#line 237 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = _sql_create_list();
			_sql_add_tail((yyval), (yyvsp[0]));
		}
#line 1989 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 22:
#line 242 "sqlgrammar.y" /* yacc.c:1646  */
    {
			_sql_add_tail((yyvsp[-2]), (yyvsp[0])); 
			(yyval) = (yyvsp[-2]);
		}
#line 1998 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 250 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(base_table_element_t, pelement);
			pelement->which = 0;
			pelement->pcolumn = (column_def_t*)(yyvsp[0]);
			(yyval) = (LITEM)pelement;
		}
#line 2009 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 257 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(base_table_element_t, pelement);
			pelement->which = 1;
			pelement->pconstraint = (table_constraint_def_t*)(yyvsp[0]);
			(yyval) = (LITEM)pelement;
		}
#line 2020 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 25:
#line 267 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(column_def_t, pcolumn);
			pcolumn->pcolumn = (column_t)(yyvsp[-2]);
			pcolumn->ptype = (data_type_t*)(yyvsp[-1]);
			pcolumn->popt = (yyvsp[0]);
			(yyval) = (LITEM)pcolumn;
		}
#line 2032 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 278 "sqlgrammar.y" /* yacc.c:1646  */
    {
		(yyval) = (LITEM)NULL;
	}
#line 2040 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 282 "sqlgrammar.y" /* yacc.c:1646  */
    {
			_sql_add_tail((yyvsp[-1]), (yyvsp[0])); 
			(yyval) = (yyvsp[-1]);
	}
#line 2049 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 290 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(column_def_opt_t, pdef);
			pdef->which = 0;
			pdef->notnull = NOT_NULLX;
			(yyval) = (LITEM)pdef;
		}
#line 2060 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 29:
#line 297 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(column_def_opt_t, pdef);
			pdef->which = 0;
			pdef->notnull = NOT_NULLX_UNIQUE;
			(yyval) = (LITEM)pdef;
		}
#line 2071 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 30:
#line 304 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(column_def_opt_t, pdef);
			pdef->which = 0;
			pdef->notnull = NOT_NULLX_PKEY;
			(yyval) = (LITEM)pdef;
		}
#line 2082 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 31:
#line 311 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(column_def_opt_t, pdef);
			pdef->which = 1;
			pdef->pdefault = sql_alloc(default_t);
			pdef->pdefault->which = 0;
			pdef->pdefault->pliteral = (literal_t*)(yyvsp[0]);
			(yyval) = (LITEM)pdef;
		}
#line 2095 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 32:
#line 320 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(column_def_opt_t, pdef);
			pdef->which = 1;
			pdef->pdefault = sql_alloc(default_t);
			pdef->pdefault->which = 1;
			pdef->pdefault->nullx = IS_NULLX;
			(yyval) = (LITEM)pdef;
		}
#line 2108 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 33:
#line 329 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(column_def_opt_t, pdef);
			pdef->which = 1;
			pdef->pdefault = sql_alloc(default_t);
			pdef->pdefault->which = 2;
			pdef->pdefault->puser = (char*)(yyvsp[0]);
			(yyval) = (LITEM)pdef;
		}
#line 2121 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 338 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(column_def_opt_t, pdef);
			pdef->which = 2;
			pdef->psearch = (search_condition_t*)(yyvsp[-1]);
			(yyval) = (LITEM)pdef;
		}
#line 2132 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 345 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(column_def_opt_t, pdef);
			pdef->which = 3;
			pdef->preferences = sql_alloc(references_t);
			pdef->preferences->ptable = (table_t*)(yyvsp[0]);
			(yyval) = (LITEM)pdef;
		}
#line 2144 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 353 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(column_def_opt_t, pdef);
			pdef->which = 3;
			pdef->preferences = sql_alloc(references_t);
			pdef->preferences->ptable = (table_t*)(yyvsp[-3]);
			pdef->preferences->pcolumns = (yyvsp[-1]);
			(yyval) = (LITEM)pdef;
		}
#line 2157 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 365 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(table_constraint_def_t, pdef);
			pdef->which = 0;
			pdef->punique = (yyvsp[-1]);
			(yyval) = (LITEM)pdef;
		}
#line 2168 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 38:
#line 372 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(table_constraint_def_t, pdef);
			pdef->which = 1;
			pdef->pprimarykeys = (yyvsp[-1]);
			(yyval) = (LITEM)pdef;
		}
#line 2179 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 380 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(table_constraint_def_t, pdef);
			pdef->which = 2;
			pdef->pforeignkey = sql_alloc(foreign_key_t);
			pdef->pforeignkey->pcolumns = (yyvsp[-3]);
			pdef->pforeignkey->preferences = sql_alloc(references_t);
			pdef->pforeignkey->preferences->ptable = (table_t*)(yyvsp[0]);
			(yyval) = (LITEM)pdef;
		}
#line 2193 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 391 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(table_constraint_def_t, pdef);
			pdef->which = 2;
			pdef->pforeignkey = sql_alloc(foreign_key_t);
			pdef->pforeignkey->pcolumns = (yyvsp[-6]);
			pdef->pforeignkey->preferences = sql_alloc(references_t);
			pdef->pforeignkey->preferences->ptable = (table_t*)(yyvsp[-3]);
			pdef->pforeignkey->preferences->pcolumns = (yyvsp[-1]);
			(yyval) = (LITEM)pdef;
		}
#line 2208 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 41:
#line 402 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(table_constraint_def_t, pdef);
			pdef->which = 3;
			pdef->pcheck = (search_condition_t*)(yyvsp[-1]);
			(yyval) = (LITEM)pdef;
		}
#line 2219 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 412 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = _sql_create_list();
			_sql_add_tail((yyval), (yyvsp[0]));
		}
#line 2228 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 43:
#line 417 "sqlgrammar.y" /* yacc.c:1646  */
    {
			_sql_add_tail((yyvsp[-2]), (yyvsp[0])); 
			(yyval) = (yyvsp[-2]);
		}
#line 2237 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 44:
#line 426 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(view_def_t, pdef);
			pdef->ptable = (table_t*)(yyvsp[-4]);
			pdef->pcolumns = (yyvsp[-3]);
			pdef->pquery = (query_spec_t*)(yyvsp[-1]);
			pdef->withcheck = (yyvsp[0]);
		}
#line 2249 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 437 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = 0;
		}
#line 2257 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 441 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = 1;
		}
#line 2265 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 47:
#line 448 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (LITEM)NULL;
		}
#line 2273 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 48:
#line 452 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (yyvsp[-1]);
		}
#line 2281 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 49:
#line 460 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(privilege_def_t, pdef);
			pdef->pprivs = (privileges_t*)(yyvsp[-5]);
			pdef->ptable = (table_t*)(yyvsp[-3]);
			pdef->pgrantees = (yyvsp[-1]);
			pdef->withgrant = (yyvsp[0]);
		}
#line 2293 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 471 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = 0;
		}
#line 2301 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 51:
#line 475 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = 1;
		}
#line 2309 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 52:
#line 482 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(privileges_t, pprivs);
			pprivs->which = 0;
			pprivs->all = 1;
			(yyval) = (LITEM)pprivs;
		}
#line 2320 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 53:
#line 489 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(privileges_t, pprivs);
			pprivs->which = 0;
			pprivs->all = 1;
			(yyval) = (LITEM)pprivs;
		}
#line 2331 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 54:
#line 496 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(privileges_t, pprivs);
			pprivs->which = 1;
			pprivs->pops = (yyvsp[0]);
			(yyval) = (LITEM)pprivs;
		}
#line 2342 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 55:
#line 506 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = _sql_create_list();
			_sql_add_tail((yyval), (yyvsp[0]));
		}
#line 2351 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 56:
#line 511 "sqlgrammar.y" /* yacc.c:1646  */
    {
			_sql_add_tail((yyvsp[-2]), (yyvsp[0])); 
			(yyval) = (yyvsp[-2]);
		}
#line 2360 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 57:
#line 519 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(operation_t, op);
			op->type = OP_SELECT;	
		}
#line 2369 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 58:
#line 524 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(operation_t, op);
			op->type = OP_INSERT;	
		}
#line 2378 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 59:
#line 529 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(operation_t, op);
			op->type = OP_DELETE;	
		}
#line 2387 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 60:
#line 534 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(operation_t, op);
			op->type = OP_UPDATE;	
		}
#line 2396 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 61:
#line 539 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(operation_t, op);
			op->type = OP_REFERENCES;	
			op->pcolumns = (yyvsp[0]);
		}
#line 2406 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 62:
#line 549 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = _sql_create_list();
			_sql_add_tail((yyval), (yyvsp[0]));
		}
#line 2415 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 63:
#line 554 "sqlgrammar.y" /* yacc.c:1646  */
    {
			_sql_add_tail((yyvsp[-2]), (yyvsp[0])); 
			(yyval) = (yyvsp[-2]);
		}
#line 2424 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 64:
#line 562 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (LITEM)NULL;
		}
#line 2432 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 65:
#line 566 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (yyvsp[0]);
		}
#line 2440 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 66:
#line 573 "sqlgrammar.y" /* yacc.c:1646  */
    {
			yyerror(root, "Module definition not supported");
		}
#line 2448 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 81:
#line 616 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (LITEM)NULL;
		}
#line 2456 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 82:
#line 620 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (yyvsp[0]);
		}
#line 2464 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 83:
#line 628 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = _sql_create_list();
			_sql_add_tail((yyval), (yyvsp[0]));
		}
#line 2473 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 84:
#line 633 "sqlgrammar.y" /* yacc.c:1646  */
    {
			_sql_add_tail((yyvsp[-2]), (yyvsp[0]));
			(yyval) = (yyvsp[-2]);
		}
#line 2482 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 85:
#line 642 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(ordering_spec_t, spec);
			spec->colnum = NULL;
			spec->pcolumn = (column_ref_t*)(yyvsp[-1]);
			spec->ascdesc = (asc_desc_t)(yyvsp[0]);
			(yyval) = (LITEM)spec;
		}
#line 2494 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 86:
#line 650 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(ordering_spec_t, spec);
			spec->colnum = (char*)(yyvsp[-1]);
			spec->pcolumn = NULL;
			spec->ascdesc = (asc_desc_t)(yyvsp[0]);
			(yyval) = (LITEM)spec;
		}
#line 2506 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 87:
#line 661 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = ORDER_NOT_SPECIFIED;
		}
#line 2514 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 88:
#line 665 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = ORDER_ASC;
		}
#line 2522 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 89:
#line 669 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = ORDER_DESC;
		}
#line 2530 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 93:
#line 686 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = _sql_create_list();
			_sql_add_tail((yyval), (yyvsp[0]));
		}
#line 2539 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 94:
#line 691 "sqlgrammar.y" /* yacc.c:1646  */
    {
			_sql_add_tail((yyvsp[-1]), (yyvsp[0]));
			(yyval) = (yyvsp[-1]);
		}
#line 2548 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 99:
#line 710 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(sql_t, psql);
			psql->which = 2;
			psql->pstmt = (manipulative_statement_t*)(yyvsp[0]);
			(yyval) = (LITEM)psql;
		}
#line 2559 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 100:
#line 720 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(manipulative_statement_t, pstmt);
			pstmt->which = 0;
			pstmt->pdelsearched = (delete_statement_searched_t*)(yyvsp[0]);
			(yyval) = (LITEM)pstmt;
		}
#line 2570 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 101:
#line 727 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(manipulative_statement_t, pstmt);
			pstmt->which = 1;
			pstmt->pinsert = (insert_statement_t*)(yyvsp[0]);
			(yyval) = (LITEM)pstmt;
		}
#line 2581 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 102:
#line 734 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(manipulative_statement_t, pstmt);
			pstmt->which = 2;
			pstmt->pselect = (select_statement_t*)(yyvsp[0]);
			(yyval) = (LITEM)pstmt;
		}
#line 2592 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 103:
#line 741 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(manipulative_statement_t, pstmt);
			pstmt->which = 3;
			pstmt->pupdatesearched = (update_statement_searched_t*)(yyvsp[0]);
			(yyval) = (LITEM)pstmt;
		}
#line 2603 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 115:
#line 772 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(delete_statement_searched_t, pstmt);
			pstmt->ptable = (table_t*)(yyvsp[-1]);
			pstmt->pwhere = (where_clause_t*)(yyvsp[0]);
			(yyval) = (LITEM)pstmt;
		}
#line 2614 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 116:
#line 780 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(delete_statement_searched_t, pstmt);
			pstmt->ptable = (table_t*)(yyvsp[-1]);
			pstmt->pwhere = (where_clause_t*)(yyvsp[0]);
			(yyval) = (LITEM)pstmt;
		}
#line 2625 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 118:
#line 794 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(insert_statement_t, pstmt);
			pstmt->ptable = (table_t*)(yyvsp[-2]);
			pstmt->pcommalist = (yyvsp[-1]);
			pstmt->pvalues = (values_or_query_spec_t*)(yyvsp[0]);
			(yyval) = (LITEM)pstmt;
		}
#line 2637 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 119:
#line 805 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(values_or_query_spec_t, pvalues);
			pvalues->which = 0;
			pvalues->patoms = (yyvsp[-1]);
			(yyval) = (LITEM)pvalues;
		}
#line 2648 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 120:
#line 812 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(values_or_query_spec_t, pvalues);
			pvalues->which = 1;
			pvalues->pquery = (query_spec_t*)(yyvsp[0]);
			(yyval) = (LITEM)pvalues;
		}
#line 2659 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 121:
#line 822 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = _sql_create_list();
			_sql_add_tail((yyval), (yyvsp[0]));
		}
#line 2668 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 122:
#line 827 "sqlgrammar.y" /* yacc.c:1646  */
    {
			_sql_add_tail((yyvsp[-2]), (yyvsp[0]));
			(yyval) = (yyvsp[-2]);
		}
#line 2677 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 123:
#line 835 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (yyvsp[0]);
		}
#line 2685 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 124:
#line 839 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (LITEM)NULL;
		}
#line 2693 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 127:
#line 856 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(select_statement_t, pstmt);
			pstmt->alldistinct = (yyvsp[-4]);
			pstmt->pselection = (selection_t*)(yyvsp[-3]);
			pstmt->pcommalist = (yyvsp[-1]);
			pstmt->ptable = (table_exp_t*)(yyvsp[0]);
			(yyval) = (LITEM)pstmt;
		}
#line 2706 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 128:
#line 868 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = SEL_NOT_SPECIFIED;
		}
#line 2714 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 129:
#line 872 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = SEL_ALL;
		}
#line 2722 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 130:
#line 876 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = SEL_DISTINCT;
		}
#line 2730 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 133:
#line 888 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = _sql_create_list();
			_sql_add_tail((yyval), (yyvsp[0]));
		}
#line 2739 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 134:
#line 893 "sqlgrammar.y" /* yacc.c:1646  */
    {
			_sql_add_tail((yyvsp[-2]), (yyvsp[0]));
			(yyval) = (yyvsp[-2]);
		}
#line 2748 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 135:
#line 901 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(assignment_t, pass);
			pass->pcolumn = (column_t)(yyvsp[-2]);
			pass->pscalar = (scalar_exp_t*)(yyvsp[0]);
			(yyval) = (LITEM)pass;
		}
#line 2759 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 136:
#line 908 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(assignment_t, pass);
			pass->pcolumn = (column_t)(yyvsp[-2]);
			pass->pscalar = (scalar_exp_t*)NULL;
			(yyval) = (LITEM)pass;
		}
#line 2770 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 137:
#line 918 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(update_statement_searched_t, pupdate);
			pupdate->ptable = (table_t*)(yyvsp[-3]);
			pupdate->plist = (yyvsp[-1]);
			pupdate->pwhereclause = (where_clause_t*)(yyvsp[0]);
			(yyval) = (LITEM)pupdate;
		}
#line 2782 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 138:
#line 929 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = _sql_create_list();
			_sql_add_tail((yyval), (yyvsp[0]));
		}
#line 2791 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 139:
#line 934 "sqlgrammar.y" /* yacc.c:1646  */
    {
			_sql_add_tail((yyvsp[-2]), (yyvsp[0]));
			(yyval) = (yyvsp[-2]);
		}
#line 2800 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 140:
#line 942 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (yyvsp[0]);
		}
#line 2808 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 141:
#line 949 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (LITEM)NULL;
		}
#line 2816 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 142:
#line 953 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (yyvsp[0]);
		}
#line 2824 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 143:
#line 961 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(sql_t, psql);
			psql->which = 3;
			psql->pquery = (query_spec_t*)(yyvsp[0]);
			(yyval) = (LITEM)psql;
		}
#line 2835 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 144:
#line 971 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(query_exp_t, pquery);
			pquery->pterm = (query_term_t*)(yyvsp[0]);
			(yyval) = (LITEM)pquery;
		}
#line 2845 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 145:
#line 977 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(query_exp_t, pquery);
			pquery->pterm = (query_term_t*)(yyvsp[0]);
			pquery->pexp = (query_exp_t*)(yyvsp[-2]);
			(yyval) = (LITEM)pquery;
		}
#line 2856 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 146:
#line 984 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(query_exp_t, pquery);
			pquery->pterm = (query_term_t*)(yyvsp[0]);
			pquery->pexp = (query_exp_t*)(yyvsp[-3]);
			(yyval) = (LITEM)pquery;
		}
#line 2867 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 147:
#line 994 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(query_term_t, pterm);
			pterm->which = 0;
			pterm->pspec = (query_spec_t*)(yyvsp[0]);
			(yyval) = (LITEM)pterm;
		}
#line 2878 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 148:
#line 1001 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(query_term_t, pterm);
			pterm->which = 1;
			pterm->pexp = (void*)(yyvsp[-1]);
			(yyval) = (LITEM)pterm;
		}
#line 2889 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 149:
#line 1012 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(query_spec_t, pspec);
			pspec->alldistinct = (yyvsp[-3]);
			pspec->pselection = (yyvsp[-2]);
			pspec->pexp = (table_exp_t*)(yyvsp[-1]);
			pspec->porderby = (yyvsp[0]);
			(yyval) = (LITEM)pspec;
		}
#line 2902 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 150:
#line 1024 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (yyvsp[0]);
		}
#line 2910 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 151:
#line 1034 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(table_exp_t, ptable);
			ptable->pfrom = (from_clause_t*)(yyvsp[-3]);
			ptable->pwhere = (void*)(yyvsp[-2]);
			ptable->pgroup = (yyvsp[-1]);
			ptable->phaving = (having_clause_t*)(yyvsp[0]);
			(yyval) = (LITEM)ptable;
		}
#line 2923 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 152:
#line 1046 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(from_clause_t, pfrom);
			pfrom->which = 0;
			pfrom->plist = (yyvsp[0]);
			(yyval) = (LITEM)pfrom;
		}
#line 2934 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 153:
#line 1053 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(from_clause_t, pfrom);
			pfrom->which = 1;
			pfrom->pjoin = sql_alloc(table_join_t);
			pfrom->pjoin->ptable = (table_ref_t*)(yyvsp[-1]);
			pfrom->pjoin->pjoin = (join_ref_t*)(yyvsp[0]);
			(yyval) = (LITEM)pfrom;
		}
#line 2947 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 154:
#line 1066 "sqlgrammar.y" /* yacc.c:1646  */
    {
			join_ref_t *pref = (join_ref_t*)(yyvsp[0]);
			pref->type = JOIN_INNER;
			(yyval) = (LITEM)pref;
		}
#line 2957 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 155:
#line 1072 "sqlgrammar.y" /* yacc.c:1646  */
    {
			join_ref_t *pref = (join_ref_t*)(yyvsp[0]);
			pref->type = JOIN_LEFT_OUTER;
			(yyval) = (LITEM)pref;
		}
#line 2967 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 156:
#line 1078 "sqlgrammar.y" /* yacc.c:1646  */
    {
			join_ref_t *pref = (join_ref_t*)(yyvsp[0]);
			pref->type = JOIN_RIGHT_OUTER;
			(yyval) = (LITEM)pref;
		}
#line 2977 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 157:
#line 1084 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(join_ref_t, pref);
			pref->type = JOIN_INNER;
			pref->ptable = (table_ref_t*)(yyvsp[0]);
			(yyval) = (LITEM)pref;
		}
#line 2988 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 158:
#line 1091 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(join_ref_t, pref);
			pref->type = JOIN_INNER;
			pref->ptable = (table_ref_t*)(yyvsp[-1]);
			pref->pjoin = (join_ref_t*)(yyvsp[0]);
			(yyval) = (LITEM)pref;
		}
#line 3000 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 159:
#line 1099 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(join_ref_t, pref);
			pref->type = JOIN_INNER;
			pref->ptable = (table_ref_t*)(yyvsp[-2]);
			pref->psearch = (void*)(yyvsp[0]);
			(yyval) = (LITEM)pref;
		}
#line 3012 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 160:
#line 1107 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(join_ref_t, pref);
			pref->type = JOIN_INNER;
			pref->ptable = (table_ref_t*)(yyvsp[-3]);
			pref->psearch = (void*)(yyvsp[-1]);
			pref->pjoin = (join_ref_t*)(yyvsp[0]);
			(yyval) = (LITEM)pref;
		}
#line 3025 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 161:
#line 1119 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = _sql_create_list();
			_sql_add_tail((yyval), (yyvsp[0]));
		}
#line 3034 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 162:
#line 1124 "sqlgrammar.y" /* yacc.c:1646  */
    {
			_sql_add_tail((yyvsp[-2]), (yyvsp[0]));
			(yyval) = (yyvsp[-2]);
		}
#line 3043 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 163:
#line 1132 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(table_ref_t, pref);
			pref->ptable = (table_t*)(yyvsp[0]);
			pref->which = -1;
			(yyval) = (LITEM)pref;
		}
#line 3054 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 164:
#line 1139 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(table_ref_t, pref);
			pref->ptable = (table_t*)(yyvsp[-1]);
			pref->which = 0;
			pref->alias = (char*)(yyvsp[0]);
			(yyval) = (LITEM)pref;
		}
#line 3066 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 165:
#line 1147 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(table_ref_t, pref);
			pref->ptable = (table_t*)(yyvsp[-2]);
			pref->which = 0;
			pref->alias = (char*)(yyvsp[0]);
			(yyval) = (LITEM)pref;
		}
#line 3078 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 166:
#line 1155 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(table_ref_t, pref);
			pref->ptable = (table_t*)(yyvsp[-1]);
			pref->which = 1;
			pref->prange = (range_variable_t)(yyvsp[0]);
			(yyval) = (LITEM)pref;
		}
#line 3090 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 167:
#line 1166 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (yyvsp[0]);
		}
#line 3098 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 168:
#line 1173 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (LITEM)NULL;
		}
#line 3106 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 169:
#line 1177 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (yyvsp[0]);
		}
#line 3114 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 170:
#line 1184 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = _sql_create_list();
			_sql_add_tail((yyval), (yyvsp[0]));
		}
#line 3123 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 171:
#line 1189 "sqlgrammar.y" /* yacc.c:1646  */
    {
			_sql_add_tail((yyvsp[-2]), (yyvsp[0]));
			(yyval) = (yyvsp[-2]);
		}
#line 3132 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 172:
#line 1197 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (LITEM)NULL;
		}
#line 3140 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 173:
#line 1201 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (yyvsp[0]);
		}
#line 3148 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 174:
#line 1210 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(search_condition_t, psearch);
			psearch->psearch1 = (search_condition_t*)(yyvsp[-2]);
			psearch->psearch2 = (search_condition_t*)(yyvsp[0]);
			psearch->searchop = SEARCHOP_OR;
			(yyval) = (LITEM)psearch;
		}
#line 3160 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 175:
#line 1218 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(search_condition_t, psearch);
			psearch->psearch1 = (search_condition_t*)(yyvsp[-2]);
			psearch->psearch2 = (search_condition_t*)(yyvsp[0]);
			psearch->searchop = SEARCHOP_AND;
			(yyval) = (LITEM)psearch;
		}
#line 3172 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 176:
#line 1226 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(search_condition_t, psearch);
			psearch->psearch2 = (search_condition_t*)(yyvsp[0]);
			psearch->searchop = SEARCHOP_NOT;
			(yyval) = (LITEM)psearch;
		}
#line 3183 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 177:
#line 1233 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(search_condition_t, psearch);
			psearch->psearch1 = (search_condition_t*)(yyvsp[-1]);
			psearch->useparens = 1;
			(yyval) = (LITEM)psearch;
		}
#line 3194 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 178:
#line 1240 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(search_condition_t, psearch);
			psearch->ppredicate = (predicate_t*)(yyvsp[0]);
			(yyval) = (LITEM)psearch;
		}
#line 3204 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 179:
#line 1249 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(predicate_t, ppred);
			ppred->which = 0;
			ppred->pcomparison = (comparison_predicate_t*)(yyvsp[0]);
			(yyval) = (LITEM)ppred;
		}
#line 3215 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 180:
#line 1256 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(predicate_t, ppred);
			ppred->which = 1;
			ppred->pin = (in_predicate_t*)(yyvsp[0]);
			(yyval) = (LITEM)ppred;
		}
#line 3226 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 181:
#line 1263 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(predicate_t, ppred);
			ppred->which = 2;
			ppred->ptestnull = (test_for_null_t*)(yyvsp[0]);
			(yyval) = (LITEM)ppred;
		}
#line 3237 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 182:
#line 1270 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(predicate_t, ppred);
			ppred->which = 3;
			ppred->pexisttest = (existence_test_t*)(yyvsp[0]);
			(yyval) = (LITEM)ppred;
		}
#line 3248 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 183:
#line 1277 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(predicate_t, ppred);
			ppred->which = 4;
			ppred->pbetween = (between_predicate_t*)(yyvsp[0]);
			(yyval) = (LITEM)ppred;
		}
#line 3259 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 184:
#line 1284 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(predicate_t, ppred);
			ppred->which = 5;
			ppred->plike = (like_predicate_t*)(yyvsp[0]);
			(yyval) = (LITEM)ppred;
		}
#line 3270 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 185:
#line 1291 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(predicate_t, ppred);
			ppred->which = 6;
			ppred->panyorall = (any_or_all_predicate_t*)(yyvsp[0]);
			(yyval) = (LITEM)ppred;
		}
#line 3281 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 186:
#line 1301 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(comparison_predicate_t, ppred);
			ppred->pscalar1 = (scalar_exp_t*)(yyvsp[-2]);
			ppred->pcomparison = (char*)(yyvsp[-1]);
			ppred->pscalar2 = (scalar_exp_t*)(yyvsp[0]);
			ppred->join_type = JOIN_INNER;
			(yyval) = (LITEM)ppred;
		}
#line 3294 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 187:
#line 1310 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(comparison_predicate_t, ppred);
			ppred->pscalar1 = (scalar_exp_t*)(yyvsp[-2]);
			ppred->pcomparison = (char*)(yyvsp[-1]);
			ppred->psubquery = (subquery_t*)(yyvsp[0]);
			ppred->join_type = JOIN_INNER;
			(yyval) = (LITEM)ppred;
		}
#line 3307 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 188:
#line 1322 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(between_predicate_t, ppred);
			ppred->pscalar1 = (scalar_exp_t*)(yyvsp[-5]);
			ppred->not = 1;
			ppred->pscalar2 = (scalar_exp_t*)(yyvsp[-2]);
			ppred->pscalar3 = (scalar_exp_t*)(yyvsp[0]);
			(yyval) = (LITEM)ppred;
		}
#line 3320 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 189:
#line 1331 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(between_predicate_t, ppred);
			ppred->pscalar1 = (scalar_exp_t*)(yyvsp[-4]);
			ppred->not = 0;
			ppred->pscalar2 = (scalar_exp_t*)(yyvsp[-2]);
			ppred->pscalar3 = (scalar_exp_t*)(yyvsp[0]);
			(yyval) = (LITEM)ppred;
		}
#line 3333 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 190:
#line 1343 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(like_predicate_t, ppred);
			ppred->pscalar = (scalar_exp_t*)(yyvsp[-4]);
			ppred->not = 1;
			ppred->patom = (atom_t*)(yyvsp[-1]);
			ppred->pescape = (atom_t*)(yyvsp[0]);
			(yyval) = (LITEM)ppred;
		}
#line 3346 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 191:
#line 1352 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(like_predicate_t, ppred);
			ppred->pscalar = (scalar_exp_t*)(yyvsp[-3]);
			ppred->not = 0;
			ppred->patom = (atom_t*)(yyvsp[-1]);
			ppred->pescape = (atom_t*)(yyvsp[0]);
			(yyval) = (LITEM)ppred;
		}
#line 3359 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 192:
#line 1364 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (LITEM)NULL;
		}
#line 3367 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 193:
#line 1368 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (yyvsp[0]);
		}
#line 3375 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 194:
#line 1375 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(test_for_null_t, ptest);
			ptest->pcolumn = (column_ref_t*)(yyvsp[-3]);
			ptest->isnull = 0;
			(yyval) = (LITEM)ptest;
		}
#line 3386 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 195:
#line 1382 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(test_for_null_t, ptest);
			ptest->pcolumn = (column_ref_t*)(yyvsp[-2]);
			ptest->isnull = 1;
			(yyval) = (LITEM)ptest;
		}
#line 3397 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 196:
#line 1392 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(in_predicate_t, pin);
			pin->pscalar = (scalar_exp_t*)(yyvsp[-3]);
			pin->not = 1; 
			pin->which = 0;
			pin->psubquery = (subquery_t*)(yyvsp[0]);
			(yyval) = (LITEM)pin;
		}
#line 3410 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 197:
#line 1401 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(in_predicate_t, pin);
			pin->pscalar = (scalar_exp_t*)(yyvsp[-2]);
			pin->not = 0; 
			pin->which = 0;
			pin->psubquery = (subquery_t*)(yyvsp[0]);
			(yyval) = (LITEM)pin;
		}
#line 3423 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 198:
#line 1410 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(in_predicate_t, pin);
			pin->pscalar = (scalar_exp_t*)(yyvsp[-5]);
			pin->not = 1; 
			pin->which = 1;
			pin->patomlist = (yyvsp[-1]);
			(yyval) = (LITEM)pin;
		}
#line 3436 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 199:
#line 1419 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(in_predicate_t, pin);
			pin->pscalar = (scalar_exp_t*)(yyvsp[-4]);
			pin->not = 1; 
			pin->which = 1;
			pin->patomlist = (yyvsp[-1]);
			(yyval) = (LITEM)pin;
		}
#line 3449 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 200:
#line 1431 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = _sql_create_list();
			_sql_add_tail((yyval), (yyvsp[0]));
		}
#line 3458 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 201:
#line 1436 "sqlgrammar.y" /* yacc.c:1646  */
    {
			_sql_add_tail((yyvsp[-2]), (yyvsp[0]));
			(yyval) = (yyvsp[-2]);
		}
#line 3467 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 202:
#line 1444 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(any_or_all_predicate_t, pall);
			pall->pscalar = (scalar_exp_t*)(yyvsp[-3]);
			pall->comparison = (char*)(yyvsp[-2]);
			pall->anyallsome = (yyvsp[-1]);
			pall->psubquery = (subquery_t*)(yyvsp[0]);
			(yyval) = (LITEM)pall;
		}
#line 3480 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 203:
#line 1456 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = PRED_ANY;
		}
#line 3488 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 204:
#line 1460 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = PRED_ALL;
		}
#line 3496 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 205:
#line 1464 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = PRED_SOME;
		}
#line 3504 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 206:
#line 1471 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (yyvsp[0]);
		}
#line 3512 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 207:
#line 1478 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (yyvsp[-1]);
		}
#line 3520 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 208:
#line 1487 "sqlgrammar.y" /* yacc.c:1646  */
    {
			char *tmp;
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pscalar1 = (scalar_exp_t*)(yyvsp[-1]);
			tmp = __sql_alloc(strlen((char*)(yyvsp[0])) + 3);
			sprintf(tmp, "%s", (char*)(yyvsp[0]));
			pscalar->name = tmp;
			(yyval) = (LITEM)pscalar;
		}
#line 3534 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 209:
#line 1497 "sqlgrammar.y" /* yacc.c:1646  */
    {
			char *tmp;
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pscalar1 = (scalar_exp_t*)(yyvsp[-2]);
			tmp = __sql_alloc(strlen((char*)(yyvsp[0])) + 3);
			sprintf(tmp, "%s", (char*)(yyvsp[0]));
			pscalar->name = tmp;
			(yyval) = (LITEM)pscalar;
		}
#line 3548 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 210:
#line 1507 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pscalar1 = (scalar_exp_t*)(yyvsp[-2]);
			pscalar->mathop = MATHOP_PLUS;
			pscalar->pscalar2 = (scalar_exp_t*)(yyvsp[0]);
			(yyval) = (LITEM)pscalar;
		}
#line 3560 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 211:
#line 1515 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pscalar1 = (scalar_exp_t*)(yyvsp[-2]);
			pscalar->mathop = MATHOP_MINUS;
			pscalar->pscalar2 = (scalar_exp_t*)(yyvsp[0]);
			(yyval) = (LITEM)pscalar;
		}
#line 3572 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 212:
#line 1523 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pscalar1 = (scalar_exp_t*)(yyvsp[-2]);
			pscalar->mathop = MATHOP_MULT;
			pscalar->pscalar2 = (scalar_exp_t*)(yyvsp[0]);
			(yyval) = (LITEM)pscalar;
		}
#line 3584 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 213:
#line 1531 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pscalar1 = (scalar_exp_t*)(yyvsp[-2]);
			pscalar->mathop = MATHOP_DIVIDE;
			pscalar->pscalar2 = (scalar_exp_t*)(yyvsp[0]);
			(yyval) = (LITEM)pscalar;
		}
#line 3596 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 214:
#line 1539 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pscalar1 = (scalar_exp_t*)(yyvsp[0]);
			pscalar->mathop = MATHOP_PLUS;
			pscalar->unary = 1;
			(yyval) = (LITEM)pscalar;
		}
#line 3608 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 215:
#line 1547 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pscalar1 = (scalar_exp_t*)(yyvsp[0]);
			pscalar->mathop = MATHOP_MINUS;
			pscalar->unary = 1;
			(yyval) = (LITEM)pscalar;
		}
#line 3620 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 216:
#line 1555 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pliteral = (literal_t*)(yyvsp[0]);
			(yyval) = (LITEM)pscalar;
		}
#line 3630 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 217:
#line 1561 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pcolumnref = (column_ref_t*)(yyvsp[0]);
			(yyval) = (LITEM)pscalar;
		}
#line 3640 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 218:
#line 1567 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pfunction_ref = (function_ref_t*)(yyvsp[0]);
			(yyval) = (LITEM)pscalar;
		}
#line 3650 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 219:
#line 1573 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pscalar1 = (scalar_exp_t*)(yyvsp[-1]);
			(yyval) = (LITEM)pscalar;
		}
#line 3660 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 220:
#line 1582 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = _sql_create_list();
			_sql_add_tail((yyval), (yyvsp[0]));
		}
#line 3669 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 221:
#line 1587 "sqlgrammar.y" /* yacc.c:1646  */
    {
			_sql_add_tail((yyvsp[-2]), (yyvsp[0]));
			(yyval) = (yyvsp[-2]);
		}
#line 3678 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 222:
#line 1595 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(atom_t, patom);
			patom->which = 0;
			patom->pparam = (parameter_ref_t*)(yyvsp[0]);
			(yyval) = (LITEM)patom;
		}
#line 3689 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 223:
#line 1602 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(atom_t, patom);
			patom->which = 1;
			patom->pliteral = (literal_t*)(yyvsp[0]);
			(yyval) = (LITEM)patom;
		}
#line 3700 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 224:
#line 1609 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(atom_t, patom);
			patom->which = 2;
			patom->user = 1;
			(yyval) = (LITEM)patom;
		}
#line 3711 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 228:
#line 1625 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(function_ref_t, pfunc);
			pfunc->name = (char*)(yyvsp[-3]);
			pfunc->asterisk = 1;
			(yyval) = (LITEM)pfunc;
		}
#line 3722 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 229:
#line 1632 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(function_ref_t, pfunc);
			pfunc->name = (char*)(yyvsp[-4]);
			pfunc->distinct = 1;
			pfunc->pcolumn = (column_ref_t*)(yyvsp[-1]);
			(yyval) = (LITEM)pfunc;
		}
#line 3734 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 230:
#line 1640 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(function_ref_t, pfunc);
			pfunc->name = (char*)(yyvsp[-3]);
			pfunc->pcolumn = (column_ref_t*)(yyvsp[-1]);
			(yyval) = (LITEM)pfunc;
		}
#line 3745 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 231:
#line 1647 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(function_ref_t, pfunc);
			pfunc->name = (char*)(yyvsp[-4]);
			pfunc->all = 1;
			pfunc->pscalar = (column_ref_t*)(yyvsp[-1]);
			(yyval) = (LITEM)pfunc;
		}
#line 3757 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 232:
#line 1655 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(function_ref_t, pfunc);
			pfunc->name = (char*)(yyvsp[-3]);
			pfunc->pscalar = (column_ref_t*)(yyvsp[-1]);
			(yyval) = (LITEM)pfunc;
		}
#line 3768 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 233:
#line 1662 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(function_ref_t, pfunc);
			pfunc->name = (char*)(yyvsp[-2]);
			(yyval) = (LITEM)pfunc;
		}
#line 3778 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 234:
#line 1671 "sqlgrammar.y" /* yacc.c:1646  */
    {
			(yyval) = (yyvsp[0]);
		}
#line 3786 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 235:
#line 1675 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(literal_t, plit);
			plit->l = atoi((const char*)(yyvsp[0]));
			plit->which = 1;
			(yyval) = (LITEM)plit;
		}
#line 3797 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 236:
#line 1682 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(literal_t, plit);
			plit->d = atof((const char *)(yyvsp[0]));
			plit->which = 2;
			(yyval) = (LITEM)plit;
		}
#line 3808 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 237:
#line 1689 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(literal_t, plit);
			plit->string = (char*)(yyvsp[0]);
			plit->which = 3;
			(yyval) = (LITEM)plit;
		}
#line 3819 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 238:
#line 1696 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(literal_t, plit);
			plit->string = (char*)(yyvsp[-2]);
			plit->which = 4;
			(yyval) = (LITEM)plit;
		}
#line 3830 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 239:
#line 1703 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(literal_t, plit);
			plit->string = (char*)(yyvsp[0]);
			plit->which = 5;
			(yyval) = (LITEM)plit;
		}
#line 3841 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 240:
#line 1710 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(literal_t, plit);
			plit->string = (char*)(yyvsp[0]);
			plit->which = 6;
			(yyval) = (LITEM)plit;
		}
#line 3852 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 241:
#line 1721 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(literal_t, plit);
			plit->string = (char*)(yyvsp[0]);
			plit->which = 0;
			(yyval) = (LITEM)plit;
		}
#line 3863 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 242:
#line 1729 "sqlgrammar.y" /* yacc.c:1646  */
    {
			literal_t *plit = (literal_t*)(yyvsp[-1]);
			char *tmp = __sql_alloc(strlen((char*)plit->string) + strlen((char*)(yyvsp[0])) + 1);
			sprintf(tmp, "%s%s", plit->string, (char*)(yyvsp[0]));
			plit->string = tmp;
			(yyval) = (LITEM)plit;
		}
#line 3875 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 243:
#line 1741 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(table_t, ptable);
			ptable->table = (char*)(yyvsp[0]);
			(yyval) = (LITEM)ptable;
		}
#line 3885 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 244:
#line 1747 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(table_t, ptable);
			ptable->owner = (char*)(yyvsp[-2]);
			ptable->table = (char*)(yyvsp[0]);
			(yyval) = (LITEM)ptable;
		}
#line 3896 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 245:
#line 1757 "sqlgrammar.y" /* yacc.c:1646  */
    {
			trigger_name_t *pname = sql_alloc(trigger_name_t);
			pname->trigger = (char*)(yyvsp[0]);
			(yyval) = (LITEM)pname;
		}
#line 3906 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 246:
#line 1763 "sqlgrammar.y" /* yacc.c:1646  */
    {
			trigger_name_t *pname = sql_alloc(trigger_name_t);
			pname->trigger = (char*)(yyvsp[-2]);
			pname->owner = (char*)(yyvsp[0]);
			(yyval) = (LITEM)pname;
		}
#line 3917 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 247:
#line 1773 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(column_ref_t, pcol);
			pcol->column = (char*)(yyvsp[0]);
			(yyval) = (LITEM)pcol;
		}
#line 3927 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 248:
#line 1779 "sqlgrammar.y" /* yacc.c:1646  */
    {
			char *tmp;
			sql_declare(column_ref_t, pcol);
			pcol->column = (char*)(yyvsp[-1]);
			tmp = __sql_alloc(strlen((char*)(yyvsp[0])) + 3);
			sprintf(tmp, "%s", (char*)(yyvsp[0]));
			pcol->alias = tmp;
			(yyval) = (LITEM)pcol;
		}
#line 3941 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 249:
#line 1789 "sqlgrammar.y" /* yacc.c:1646  */
    {
			char *tmp;
			sql_declare(column_ref_t, pcol);
			pcol->column = (char*)(yyvsp[-2]);
			tmp = __sql_alloc(strlen((char*)(yyvsp[0])) + 3);
			sprintf(tmp, "%s", (char*)(yyvsp[0]));
			pcol->alias = tmp;
			(yyval) = (LITEM)pcol;
		}
#line 3955 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 250:
#line 1799 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(column_ref_t, pcol);
			pcol->tablename = (char*)(yyvsp[-2]);
			pcol->column = (char*)(yyvsp[0]);
			(yyval) = (LITEM)pcol;
		}
#line 3966 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 251:
#line 1806 "sqlgrammar.y" /* yacc.c:1646  */
    {
			char *tmp;
			sql_declare(column_ref_t, pcol);
			pcol->tablename = (char*)(yyvsp[-3]);
			pcol->column = (char*)(yyvsp[-1]);
			tmp = __sql_alloc(strlen((char*)(yyvsp[0])) + 3);
			sprintf(tmp, "%s", (char*)(yyvsp[0]));
			pcol->alias = tmp;
			(yyval) = (LITEM)pcol;
		}
#line 3981 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 252:
#line 1817 "sqlgrammar.y" /* yacc.c:1646  */
    {
			char *tmp;
			sql_declare(column_ref_t, pcol);
			pcol->tablename = (char*)(yyvsp[-4]);
			pcol->column = (char*)(yyvsp[-2]);
			tmp = __sql_alloc(strlen((char*)(yyvsp[0])) + 3);
			sprintf(tmp, "%s", (char*)(yyvsp[0]));
			pcol->alias = tmp;
			(yyval) = (LITEM)pcol;
		}
#line 3996 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 253:
#line 1828 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(column_ref_t, pcol);
			pcol->tablename = (char*)(yyvsp[-4]);
			pcol->column = (char*)(yyvsp[-2]);
			pcol->alias = (char*)(yyvsp[0]);
			(yyval) = (LITEM)pcol;
		}
#line 4008 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 260:
#line 1848 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(data_type_t, ptype);
			ptype->type = DT_CHAR;
			ptype->scale = 1;
			(yyval) = (LITEM)ptype;
		}
#line 4019 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 261:
#line 1855 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(data_type_t, ptype);
			ptype->type = DT_CHAR;
			ptype->scale = atoi((char*)(yyvsp[-1]));
			(yyval) = (LITEM)ptype;
		}
#line 4030 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 262:
#line 1862 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(data_type_t, ptype);
			ptype->type = DT_NUMERIC;
			(yyval) = (LITEM)ptype;
		}
#line 4040 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 263:
#line 1868 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(data_type_t, ptype);
			ptype->type = DT_NUMERIC;
			ptype->scale = atoi((char*)(yyvsp[-1]));
			(yyval) = (LITEM)ptype;
		}
#line 4051 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 264:
#line 1875 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(data_type_t, ptype);
			ptype->type = DT_NUMERIC;
			ptype->scale = atoi((char*)(yyvsp[-3]));
			ptype->precision = atoi((char*)(yyvsp[-1]));
			(yyval) = (LITEM)ptype;
		}
#line 4063 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 265:
#line 1883 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(data_type_t, ptype);
			ptype->type = DT_DECIMAL;
			(yyval) = (LITEM)ptype;
		}
#line 4073 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 266:
#line 1889 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(data_type_t, ptype);
			ptype->type = DT_DECIMAL;
			ptype->scale = atoi((char*)(yyvsp[-1]));
			(yyval) = (LITEM)ptype;
		}
#line 4084 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 267:
#line 1896 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(data_type_t, ptype);
			ptype->type = DT_DECIMAL;
			ptype->scale = atoi((char*)(yyvsp[-3]));
			ptype->precision = atoi((char*)(yyvsp[-1]));
			(yyval) = (LITEM)ptype;
		}
#line 4096 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 268:
#line 1904 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(data_type_t, ptype);
			ptype->type = DT_INTEGER;
			(yyval) = (LITEM)ptype;
		}
#line 4106 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 269:
#line 1910 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(data_type_t, ptype);
			ptype->type = DT_SMALLINT;
			(yyval) = (LITEM)ptype;
		}
#line 4116 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 270:
#line 1916 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(data_type_t, ptype);
			ptype->type = DT_FLOAT;
			(yyval) = (LITEM)ptype;
		}
#line 4126 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 271:
#line 1922 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(data_type_t, ptype);
			ptype->type = DT_REAL;
			(yyval) = (LITEM)ptype;
		}
#line 4136 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 272:
#line 1928 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(data_type_t, ptype);
			ptype->type = DT_DOUBLE;
			(yyval) = (LITEM)ptype;
		}
#line 4146 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 273:
#line 1934 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(data_type_t, ptype);
			ptype->type = DT_VARCHAR;
			ptype->scale = 1;
			(yyval) = (LITEM)ptype;
		}
#line 4157 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 274:
#line 1941 "sqlgrammar.y" /* yacc.c:1646  */
    {
			sql_declare(data_type_t, ptype);
			ptype->type = DT_VARCHAR;
			ptype->scale = atoi((char*)(yyvsp[-1]));
			(yyval) = (LITEM)ptype;
		}
#line 4168 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 275:
#line 1952 "sqlgrammar.y" /* yacc.c:1646  */
    {
				(yyval) = (yyvsp[0]);
			}
#line 4176 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 276:
#line 1958 "sqlgrammar.y" /* yacc.c:1646  */
    {
				(yyval) = (yyvsp[0]);
			}
#line 4184 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 277:
#line 1964 "sqlgrammar.y" /* yacc.c:1646  */
    {
				(yyval) = (yyvsp[0]);
			}
#line 4192 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 278:
#line 1971 "sqlgrammar.y" /* yacc.c:1646  */
    {
				(yyval) = (yyvsp[0]);
			}
#line 4200 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 279:
#line 1977 "sqlgrammar.y" /* yacc.c:1646  */
    {
				(yyval) = (yyvsp[0]);
			}
#line 4208 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 280:
#line 1983 "sqlgrammar.y" /* yacc.c:1646  */
    {
				(yyval) = (yyvsp[0]);
			}
#line 4216 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;

  case 281:
#line 1989 "sqlgrammar.y" /* yacc.c:1646  */
    {
				(yyval) = (yyvsp[0]);
			}
#line 4224 "sqlgrammar.tab.c" /* yacc.c:1646  */
    break;


#line 4228 "sqlgrammar.tab.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (root, YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (root, yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, root);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp, root);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (root, YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, root);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp, root);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 1994 "sqlgrammar.y" /* yacc.c:1906  */

