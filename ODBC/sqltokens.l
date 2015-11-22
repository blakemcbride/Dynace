%{


#include "sqlgrammar.h"
#include "sqlstructs.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#pragma warning (disable:4013)

int sql_lineno = 1;
void yyerror(void *, char *s);
int nesting_level = 0;
extern void *  __sql_alloc(size_t size);
%}
	/* MKS needs the next line to increase the NFA table */
%e 1200

%x ODBCFUNC

%%

	/* literal keyword tokens */

ADA		{ return ADA; }
ALL		{ return ALL; }
AND		{ return AND; }
AVG		{
			char	*str = strcpy((char*)__sql_alloc(strlen(yytext) + 1), yytext);
			yylval = (YYSTYPE) str;
			return AMMSC;
		}
MIN		{
			char	*str = strcpy((char*)__sql_alloc(strlen(yytext) + 1), yytext);
			yylval = (YYSTYPE) str;
			return AMMSC;
		}
MAX		{
			char	*str = strcpy((char*)__sql_alloc(strlen(yytext) + 1), yytext);
			yylval = (YYSTYPE) str;
			return AMMSC;
		}
SUM		{
			char	*str = strcpy((char*)__sql_alloc(strlen(yytext) + 1), yytext);
			yylval = (YYSTYPE) str;
			return AMMSC;
		}
COUNT		{
			char	*str = strcpy((char*)__sql_alloc(strlen(yytext) + 1), yytext);
			yylval = (YYSTYPE) str;
			return AMMSC;
		}
SYSDATE		{
			char	*str = strcpy((char*)__sql_alloc(strlen(yytext) + 1), yytext);
			yylval = (YYSTYPE) str;
			return AMMSC;
		}

ANY			{ return ANY; }
AS			{ return AS; }
ASC			{ return ASC; }
AUTHORIZATION	{ return AUTHORIZATION; }
BETWEEN		{ return BETWEEN; }
BY			{ return BY; }
C			{ return C; }
CHAR(ACTER)?	{ return CHARACTER; }
CHECK		{ return CHECK; }
CLOSE		{ return CLOSE; }
COBOL		{ return COBOL; }
COMMIT		{ return COMMIT; }
CREATE		{ return CREATE; }
CURRENT		{ return CURRENT; }
CURSOR		{ return CURSOR; }
DECIMAL		{ return DECIMAL2; }
DECLARE		{ return DECLARE; }
DEFAULT		{ return DEFAULT; }
DELETE		{ return DELETE2; }
DESC		{ return DESC; }
DISTINCT	{ return DISTINCT; }
DOUBLE		{ return DOUBLE2; }
DROP		{ return DROP; }
ESCAPE		{ return ESCAPE; }
EXISTS		{ return EXISTS; }
FETCH		{ return FETCH; }
FLOAT		{ return FLOAT2; }
FOR			{ return FOR; }
FOREIGN		{ return FOREIGN; }
FORTRAN		{ return FORTRAN; }
FROM		{ return FROM; }
GRANT		{ return GRANT; }
GROUP		{ return GROUP; }
HAVING		{ return HAVING; }
IN			{ return IN2; }
INDICATOR	{ return INDICATOR; }
INNER		{ return INNER; }
INSERT		{ return INSERT; }
INT(EGER)?	{ return INTEGER; }
INTO		{ return INTO; }
IS			{ return IS; }
JOIN		{ return JOIN; }
KEY			{ return KEY; }
LANGUAGE	{ return LANGUAGE; }
LEFT		{ return LEFT; }
LIKE		{ return LIKE; }
MODULE		{ return MODULE; }
NOT			{ return NOT; }
NULL		{ return NULLX; }
NUMERIC		{ return NUMERIC; }
OF			{ return OF; }
ON			{ return ON; }
OPEN		{ return OPEN; }
OPTION		{ return OPTION; }
OR			{ return OR; }
ORDER		{ return ORDER; }
OUTER		{ return OUTER; }
PASCAL		{ return PASCAL2; }
PLI			{ return PLI; }
PRECISION	{ return PRECISION; }
PRIMARY		{ return PRIMARY; }
PRIVILEGES	{ return PRIVILEGES; }	
PROCEDURE	{ return PROCEDURE; }
PUBLIC		{ return PUBLIC; }
REAL		{ return REAL; }
REFERENCES	{ return REFERENCES; }
RIGHT		{ return RIGHT; }
ROLLBACK	{ return ROLLBACK; }
SCHEMA		{ return SCHEMA; }
SELECT		{ return SELECT; }
SET			{ return SET; }
SMALLINT	{ return SMALLINT; }
SOME		{ return SOME; }
SQLCODE		{ return SQLCODE; }
TABLE		{ return TABLE; }
TO			{ return TO; }
TRIGGER		{ return TRIGGER; }
UNION		{ return UNION; }
UNIQUE		{ return UNIQUE; }
UPDATE		{ return UPDATE; }
USER		{ return USER; }
VALUES		{ return VALUES; }
VARCHAR		{ return VARCHAR; }
VIEW		{ return VIEW; }
WHERE		{ return WHERE; }
WITH		{ return WITH; }
WORK		{ return WORK; }

	/* punctuation */

"="		{ 
			char	*str = strcpy((char*)__sql_alloc(strlen(yytext) + 1), yytext);
			yylval = (YYSTYPE)str;
			return EQUALS; 
		}

"<>" | 
"!="	{ 
			char	*str = strcpy((char*)__sql_alloc(strlen(yytext) + 1), yytext);
			yylval = (YYSTYPE)str;
			return NOT_EQUALS; 
		}

"<"		{ 
			char	*str = strcpy((char*)__sql_alloc(strlen(yytext) + 1), yytext);
			yylval = (YYSTYPE)str;
			return LESS_THAN; 
		}

">"		{ 
			char	*str = strcpy((char*)__sql_alloc(strlen(yytext) + 1), yytext);
			yylval = (YYSTYPE)str;
			return GREATER_THAN; 
		}

"<="	{ 
			char	*str = strcpy((char*)__sql_alloc(strlen(yytext) + 1), yytext);
			yylval = (YYSTYPE)str;
			return LESS_THAN_EQ; 
		}

">="	{ 
			char	*str = strcpy((char*)__sql_alloc(strlen(yytext) + 1), yytext);
			yylval = (YYSTYPE)str;
			return GREATER_THAN_EQ; 
		}

[-+*/:(),.;]	{ 
			char	*str = strcpy((char*)__sql_alloc(strlen(yytext) + 1), yytext);
			yylval = (YYSTYPE)str;
			return yytext[0]; 
		}

	/* names */

[A-Za-z][A-Za-z0-9_]* {
			char	*str = strcpy((char*)__sql_alloc(strlen(yytext) + 1), yytext);
			yylval = (YYSTYPE)str;
			return NAME;
		}

	/* numbers */

[+-]?[0-9]+	{
			char	*str = strcpy((char*)__sql_alloc(strlen(yytext) + 1), yytext);
			yylval = (YYSTYPE)str;
			return INTNUM;
 		}

[+-]?[0-9]+"."[0-9]* |
[+-]?"."[0-9]*	|
[+-]?[0-9]+[eE][+-]?[0-9]+	|
[+-]?[0-9]+"."[0-9]*[eE][+-]?[0-9]+ |
[+-]?"."[0-9]*[eE][+-]?[0-9]+	{
			char	*str = strcpy((char*)__sql_alloc(strlen(yytext) + 1), yytext);
			yylval = (YYSTYPE)str;
			return APPROXNUM;
 		}

	/* strings */

'[1-2][0-9]{3}-[ ]*[0-9]{1,2}-[ ]*[0-9]{1,2}' |
~[1-2][0-9]{3}-[ ]*[0-9]{1,2}-[ ]*[0-9]{1,2}~ {
			char	*str = strcpy((char*)__sql_alloc(strlen(yytext) + 1), yytext);
			yylval = (YYSTYPE)str;
			return DATE_LITERAL;
		}
			
'[1-2][0-9]{3}-[ ]*[0-9]{1,2}-[ ]*[0-9]{1,2}[ ][0-9][0-9]:[ ]*[0-9][0-9]:[ ]*[0-9][0-9]' |
~[1-2][0-9]{3}-[ ]*[0-9]{1,2}-[ ]*[0-9]{1,2}[ ][0-9][0-9]:[ ]*[0-9][0-9]:[ ]*[0-9][0-9]~ {
			char	*str = strcpy((char*)__sql_alloc(strlen(yytext) + 1), yytext);
			yylval = (YYSTYPE)str;
			return TIME_LITERAL;
		}
			
'[^']*' |
~[^~]*~ {
			char	*str = strcpy((char*)__sql_alloc(strlen(yytext) + 1), yytext);
			yylval = (YYSTYPE)str;
			return STRING;
		}

	/* treat ODBC scalar function calls as string literals (these functions can nest) */
	
<INITIAL,ODBCFUNC>\{fn[[:blank:]]+ {
			if (! nesting_level++)
				BEGIN(ODBCFUNC);
			yymore();
		}

<ODBCFUNC>[^\{\}]+ {
			yymore();
		}
		
<ODBCFUNC>\}	{
			if (--nesting_level)
				yymore();
			else {
				char	*str = strcpy((char*)__sql_alloc(strlen(yytext) + 1), yytext);
				yylval = (YYSTYPE)str;
				BEGIN(INITIAL);
				return STRING;
			}
		}
		
	
'[^']*$ { 
			yyerror(NULL, "Unterminated string"); 
		}

\n		{
			sql_lineno++;
		}

[ \t\r]+	;	/* white space */

"--".*$		;	/* comment */

%%
