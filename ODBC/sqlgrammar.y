

%{


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
%}

%parse-param {void *root}

	/* symbolic tokens */

	
%token NAME
%token STRING
%token INTNUM APPROXNUM

	/* operators */

%left OR
%left AND
%left NOT
%left EQUALS
%left GREATER_THAN
%left GREATER_THAN_EQ
%left LESS_THAN
%left LESS_THAN_EQ
%left NOT_EQUALS
%left '+' '-'
%left '*' '/'
%nonassoc UMINUS

	/* literal keyword tokens */

%token ALL AMMSC ANY AS ASC AUTHORIZATION BETWEEN BY
%token CHARACTER CHECK CLOSE COMMIT CREATE CURRENT
%token CURSOR DATE_LITERAL DECIMAL2 DECLARE DEFAULT DELETE2 DESC DISTINCT DOUBLE2 DROP
%token EQUALS ESCAPE EXISTS FETCH FLOAT2 FOR FOREIGN FROM 
%token GRANT GREATER_THAN GREATER_THAN_EQ GROUP HAVING IN2 INDICATOR INNER INSERT INTEGER INTO
%token IS JOIN KEY LANGUAGE LEFT LESS_THAN LESS_THAN_EQ LIKE MODULE NOT_EQUALS NULLX NUMERIC OF ON
%token OPEN OPTION ORDER OUTER PRECISION PRIMARY PRIVILEGES PROCEDURE
%token PUBLIC REAL REFERENCES RIGHT ROLLBACK SCHEMA SELECT SET
%token SMALLINT SOME SQLCODE SYSDATE TABLE TIME_LITERAL TO TRIGGER UNION
%token UNIQUE UPDATE USER VALUES VIEW WHERE WITH WORK
%token COBOL FORTRAN PASCAL2 PLI C ADA VARCHAR

%%

root:	sql_list
		{
			*((sql_list_t**)root) = (sql_list_t*)$1;
		}
;

sql_list:
		sql ';'
		{
			$$ = _sql_create_list();
			_sql_add_tail($$, $1);
		}
	|	sql_list sql ';' 
		{
			_sql_add_tail($1, $2); 
			$$ = $1;
		}
	|	sql
		{
			$$ = _sql_create_list();
			_sql_add_tail($$, $1);
		}
;

	/* schema definition language */
	/* Note: other ``sql:'' rules appear later in the grammar */
sql:		schema
			{
				sql_declare(sql_t, psql);
				psql->which = 0;
				psql->pschema = (schema_t*)$1;
				$$ = (LITEM)psql;
			}
		|
			schema_element
			{
				sql_declare(sql_t, psql);
				psql->which = 4;
				psql->pschema_element = (schema_element_t*)$1;
				$$ = (LITEM)psql;
			}
	;
	
schema:
		CREATE SCHEMA AUTHORIZATION user opt_schema_element_list
		{
			schema_t *pschema = sql_alloc(schema_t);
			pschema->puser = (user_t*)$4;
			pschema->plist = $5;
			$$ = (LITEM)pschema;
		}
	;

opt_schema_element_list:
		/* empty */
	|	schema_element_list
		{
			$$ = $1;
		}
	;

schema_element_list:
		schema_element
		{
			$$ = _sql_create_list();
			_sql_add_tail($$, $1);
		}
	|	schema_element_list schema_element
		{
			_sql_add_tail($1, $2); 
			$$ = $1;
		}
	;

schema_element:
		base_table_def
		{
			schema_element_t *pelement = sql_alloc(schema_element_t);
			pelement->which = 0;
			pelement->ptabledef = (base_table_def_t *)$1;
			$$ = (LITEM)pelement;
		}
	|	view_def
		{
			schema_element_t *pelement = sql_alloc(schema_element_t);
			pelement->which = 1;
			pelement->pviewdef = (view_def_t *)$1;
			$$ = (LITEM)pelement;
		}
	|	privilege_def
		{
			schema_element_t *pelement = sql_alloc(schema_element_t);
			pelement->which = 2;
			pelement->pprivilegedef = (privilege_def_t *)$1;
			$$ = (LITEM)pelement;
		}
	|	drop_trigger
		{
			schema_element_t *pelement = sql_alloc(schema_element_t);
			pelement->which = 3;
			pelement->pdroptrigger = (drop_trigger_t*)$1;
			$$ = (LITEM)pelement;
		}
	|
		drop_table
		{
			schema_element_t *pelement = sql_alloc(schema_element_t);
			pelement->which = 4;
			pelement->pdroptable = (drop_table_t*)$1;
			$$ = (LITEM)pelement;
		}
	;

drop_trigger:
		DROP TRIGGER trigger_name
		{
			$$ = $3;
		}
		;

drop_table:
		DROP TABLE table
		{
			$$ = $3;
		}
		;

base_table_def:
		CREATE TABLE table '(' base_table_element_commalist ')'
		{
			base_table_def_t *pdef = sql_alloc(base_table_def_t);
			pdef->ptable = (table_t*)$3;
			pdef->plist = $5;
			$$ = (LITEM)pdef;
		}
	;

base_table_element_commalist:
		base_table_element
		{
			$$ = _sql_create_list();
			_sql_add_tail($$, $1);
		}
	|	base_table_element_commalist ',' base_table_element
		{
			_sql_add_tail($1, $3); 
			$$ = $1;
		}
	;

base_table_element:
		column_def
		{
			sql_declare(base_table_element_t, pelement);
			pelement->which = 0;
			pelement->pcolumn = (column_def_t*)$1;
			$$ = (LITEM)pelement;
		}
	|	table_constraint_def
		{
			sql_declare(base_table_element_t, pelement);
			pelement->which = 1;
			pelement->pconstraint = (table_constraint_def_t*)$1;
			$$ = (LITEM)pelement;
		}
	;

column_def:
		column data_type column_def_opt_list
		{
			sql_declare(column_def_t, pcolumn);
			pcolumn->pcolumn = (column_t)$1;
			pcolumn->ptype = (data_type_t*)$2;
			pcolumn->popt = $3;
			$$ = (LITEM)pcolumn;
		}
	;

column_def_opt_list:
		/* empty */
	{
		$$ = (LITEM)NULL;
	}
	|	column_def_opt_list column_def_opt
	{
			_sql_add_tail($1, $2); 
			$$ = $1;
	}
	;

column_def_opt:
		NOT NULLX
		{
			sql_declare(column_def_opt_t, pdef);
			pdef->which = 0;
			pdef->notnull = NOT_NULLX;
			$$ = (LITEM)pdef;
		}
	|	NOT NULLX UNIQUE
		{
			sql_declare(column_def_opt_t, pdef);
			pdef->which = 0;
			pdef->notnull = NOT_NULLX_UNIQUE;
			$$ = (LITEM)pdef;
		}
	|	NOT NULLX PRIMARY KEY
		{
			sql_declare(column_def_opt_t, pdef);
			pdef->which = 0;
			pdef->notnull = NOT_NULLX_PKEY;
			$$ = (LITEM)pdef;
		}
	|	DEFAULT literal
		{
			sql_declare(column_def_opt_t, pdef);
			pdef->which = 1;
			pdef->pdefault = sql_alloc(default_t);
			pdef->pdefault->which = 0;
			pdef->pdefault->pliteral = (literal_t*)$2;
			$$ = (LITEM)pdef;
		}
	|	DEFAULT NULLX
		{
			sql_declare(column_def_opt_t, pdef);
			pdef->which = 1;
			pdef->pdefault = sql_alloc(default_t);
			pdef->pdefault->which = 1;
			pdef->pdefault->nullx = IS_NULLX;
			$$ = (LITEM)pdef;
		}
	|	DEFAULT USER
		{
			sql_declare(column_def_opt_t, pdef);
			pdef->which = 1;
			pdef->pdefault = sql_alloc(default_t);
			pdef->pdefault->which = 2;
			pdef->pdefault->puser = (char*)$2;
			$$ = (LITEM)pdef;
		}
	|	CHECK '(' search_condition ')'
		{
			sql_declare(column_def_opt_t, pdef);
			pdef->which = 2;
			pdef->psearch = (search_condition_t*)$3;
			$$ = (LITEM)pdef;
		}
	|	REFERENCES table
		{
			sql_declare(column_def_opt_t, pdef);
			pdef->which = 3;
			pdef->preferences = sql_alloc(references_t);
			pdef->preferences->ptable = (table_t*)$2;
			$$ = (LITEM)pdef;
		}
	|	REFERENCES table '(' column_commalist ')'
		{
			sql_declare(column_def_opt_t, pdef);
			pdef->which = 3;
			pdef->preferences = sql_alloc(references_t);
			pdef->preferences->ptable = (table_t*)$2;
			pdef->preferences->pcolumns = $4;
			$$ = (LITEM)pdef;
		}
	;

table_constraint_def:
		UNIQUE '(' column_commalist ')'
		{
			sql_declare(table_constraint_def_t, pdef);
			pdef->which = 0;
			pdef->punique = $3;
			$$ = (LITEM)pdef;
		}
	|	PRIMARY KEY '(' column_commalist ')'
		{
			sql_declare(table_constraint_def_t, pdef);
			pdef->which = 1;
			pdef->pprimarykeys = $4;
			$$ = (LITEM)pdef;
		}
	|	FOREIGN KEY '(' column_commalist ')'
			REFERENCES table 
		{
			sql_declare(table_constraint_def_t, pdef);
			pdef->which = 2;
			pdef->pforeignkey = sql_alloc(foreign_key_t);
			pdef->pforeignkey->pcolumns = $4;
			pdef->pforeignkey->preferences = sql_alloc(references_t);
			pdef->pforeignkey->preferences->ptable = (table_t*)$7;
			$$ = (LITEM)pdef;
		}
	|	FOREIGN KEY '(' column_commalist ')'
			REFERENCES table '(' column_commalist ')'
		{
			sql_declare(table_constraint_def_t, pdef);
			pdef->which = 2;
			pdef->pforeignkey = sql_alloc(foreign_key_t);
			pdef->pforeignkey->pcolumns = $4;
			pdef->pforeignkey->preferences = sql_alloc(references_t);
			pdef->pforeignkey->preferences->ptable = (table_t*)$7;
			pdef->pforeignkey->preferences->pcolumns = $9;
			$$ = (LITEM)pdef;
		}
	|	CHECK '(' search_condition ')'
		{
			sql_declare(table_constraint_def_t, pdef);
			pdef->which = 3;
			pdef->pcheck = (search_condition_t*)$3;
			$$ = (LITEM)pdef;
		}
	;

column_commalist:
		column
		{
			$$ = _sql_create_list();
			_sql_add_tail($$, $1);
		}
	|	column_commalist ',' column
		{
			_sql_add_tail($1, $3); 
			$$ = $1;
		}
	;

view_def:
		CREATE VIEW table opt_column_commalist
		AS query_spec opt_with_check_option
		{
			sql_declare(view_def_t, pdef);
			pdef->ptable = (table_t*)$3;
			pdef->pcolumns = $4;
			pdef->pquery = (query_spec_t*)$6;
			pdef->withcheck = $7;
		}
	;
	
opt_with_check_option:
		/* empty */
		{
			$$ = 0;
		}
	|	WITH CHECK OPTION
		{
			$$ = 1;
		}
	;

opt_column_commalist:
		/* empty */
		{
			$$ = (LITEM)NULL;
		}
	|	'(' column_commalist ')'
		{
			$$ = $2;
		}
	;

privilege_def:
		GRANT privileges ON table TO grantee_commalist
		opt_with_grant_option
		{
			sql_declare(privilege_def_t, pdef);
			pdef->pprivs = (privileges_t*)$2;
			pdef->ptable = (table_t*)$4;
			pdef->pgrantees = $6;
			pdef->withgrant = $7;
		}
	;

opt_with_grant_option:
		/* empty */
		{
			$$ = 0;
		}
	|	WITH GRANT OPTION
		{
			$$ = 1;
		}
	;

privileges:
		ALL PRIVILEGES
		{
			sql_declare(privileges_t, pprivs);
			pprivs->which = 0;
			pprivs->all = 1;
			$$ = (LITEM)pprivs;
		}
	|	ALL
		{
			sql_declare(privileges_t, pprivs);
			pprivs->which = 0;
			pprivs->all = 1;
			$$ = (LITEM)pprivs;
		}
	|	operation_commalist
		{
			sql_declare(privileges_t, pprivs);
			pprivs->which = 1;
			pprivs->pops = $1;
			$$ = (LITEM)pprivs;
		}
	;

operation_commalist:
		operation
		{
			$$ = _sql_create_list();
			_sql_add_tail($$, $1);
		}
	|	operation_commalist ',' operation
		{
			_sql_add_tail($1, $3); 
			$$ = $1;
		}
	;

operation:
		SELECT
		{
			sql_declare(operation_t, op);
			op->type = OP_SELECT;	
		}
	|	INSERT
		{
			sql_declare(operation_t, op);
			op->type = OP_INSERT;	
		}
	|	DELETE2
		{
			sql_declare(operation_t, op);
			op->type = OP_DELETE;	
		}
	|	UPDATE opt_column_commalist
		{
			sql_declare(operation_t, op);
			op->type = OP_UPDATE;	
		}
	|	REFERENCES opt_column_commalist
		{
			sql_declare(operation_t, op);
			op->type = OP_REFERENCES;	
			op->pcolumns = $2;
		}
	;


grantee_commalist:
		grantee
		{
			$$ = _sql_create_list();
			_sql_add_tail($$, $1);
		}
	|	grantee_commalist ',' grantee
		{
			_sql_add_tail($1, $3); 
			$$ = $1;
		} 
	;

grantee:
		PUBLIC
		{
			$$ = (LITEM)NULL;
		}
	|	user
		{
			$$ = $1;
		}
	;

	/* module language */
sql:		module_def
		{
			yyerror(root, "Module definition not supported");
		}
	;

module_def:
		MODULE opt_module
		LANGUAGE lang
		AUTHORIZATION user
		opt_cursor_def_list
		procedure_def_list
	;

opt_module:
		/* empty */
	|	module
	;

lang:
		COBOL
	|	FORTRAN
	|	PASCAL2
	|	PLI
	|	C
	|	ADA
	;

opt_cursor_def_list:
		/* empty */
	|	cursor_def_list
	;

cursor_def_list:
		cursor_def
	|	cursor_def_list cursor_def
	;

cursor_def:
		DECLARE cursor CURSOR FOR query_exp opt_order_by_clause
	;

opt_order_by_clause:
		/* empty */
		{
			$$ = (LITEM)NULL;
		}
	|	ORDER BY ordering_spec_commalist
		{
			$$ = $3;
		}

	;

ordering_spec_commalist:
		ordering_spec
		{
			$$ = _sql_create_list();
			_sql_add_tail($$, $1);
		}
	|	ordering_spec_commalist ',' ordering_spec
		{
			_sql_add_tail($1, $3);
			$$ = $1;
		}

	;

ordering_spec:
		column_ref opt_asc_desc
		{
			sql_declare(ordering_spec_t, spec);
			spec->colnum = NULL;
			spec->pcolumn = (column_ref_t*)$1;
			spec->ascdesc = (asc_desc_t)$2;
			$$ = (LITEM)spec;
		}
	|	INTNUM opt_asc_desc
		{
			sql_declare(ordering_spec_t, spec);
			spec->colnum = (char*)$1;
			spec->pcolumn = NULL;
			spec->ascdesc = (asc_desc_t)$2;
			$$ = (LITEM)spec;
		}
	;

opt_asc_desc:
		/* empty */
		{
			$$ = ORDER_NOT_SPECIFIED;
		}
	|	ASC
		{
			$$ = ORDER_ASC;
		}
	|	DESC
		{
			$$ = ORDER_DESC;
		}
	;

procedure_def_list:
		procedure_def
	|	procedure_def_list procedure_def
	;

procedure_def:
		PROCEDURE procedure parameter_def_list ';'
		manipulative_statement_list
	;

manipulative_statement_list:
		manipulative_statement
		{
			$$ = _sql_create_list();
			_sql_add_tail($$, $1);
		}
	|	manipulative_statement_list manipulative_statement
		{
			_sql_add_tail($1, $2);
			$$ = $1;
		}
	;

parameter_def_list:
		parameter_def
	|	parameter_def_list parameter_def
	;

parameter_def:
		parameter data_type
	|	SQLCODE
	;

	/* manipulative statements */

sql:	manipulative_statement
		{
			sql_declare(sql_t, psql);
			psql->which = 2;
			psql->pstmt = (manipulative_statement_t*)$1;
			$$ = (LITEM)psql;
		}
	;

manipulative_statement:
		delete_statement_searched
		{
			sql_declare(manipulative_statement_t, pstmt);
			pstmt->which = 0;
			pstmt->pdelsearched = (delete_statement_searched_t*)$1;
			$$ = (LITEM)pstmt;
		}
	|	insert_statement
		{
			sql_declare(manipulative_statement_t, pstmt);
			pstmt->which = 1;
			pstmt->pinsert = (insert_statement_t*)$1;
			$$ = (LITEM)pstmt;
		}
	|	select_statement
		{
			sql_declare(manipulative_statement_t, pstmt);
			pstmt->which = 2;
			pstmt->pselect = (select_statement_t*)$1;
			$$ = (LITEM)pstmt;
		}
	|	update_statement_searched
		{
			sql_declare(manipulative_statement_t, pstmt);
			pstmt->which = 3;
			pstmt->pupdatesearched = (update_statement_searched_t*)$1;
			$$ = (LITEM)pstmt;
		}
	|	close_statement
	|	commit_statement
	|	delete_statement_positioned
	|	fetch_statement
	|	open_statement
	|	rollback_statement
	|	update_statement_positioned
	;

close_statement:
		CLOSE cursor
	;

commit_statement:
		COMMIT WORK
	;

delete_statement_positioned:
		DELETE2 FROM table WHERE CURRENT OF cursor
	|
		DELETE2 table WHERE CURRENT OF cursor
	;

delete_statement_searched:
		DELETE2 FROM table opt_where_clause
		{
			sql_declare(delete_statement_searched_t, pstmt);
			pstmt->ptable = (table_t*)$3;
			pstmt->pwhere = (where_clause_t*)$4;
			$$ = (LITEM)pstmt;
		}
	|
		DELETE2 table opt_where_clause
		{
			sql_declare(delete_statement_searched_t, pstmt);
			pstmt->ptable = (table_t*)$2;
			pstmt->pwhere = (where_clause_t*)$3;
			$$ = (LITEM)pstmt;
		}
	;

fetch_statement:
		FETCH cursor INTO target_commalist
	;

insert_statement:
		INSERT INTO table opt_column_commalist values_or_query_spec
		{
			sql_declare(insert_statement_t, pstmt);
			pstmt->ptable = (table_t*)$3;
			pstmt->pcommalist = $4;
			pstmt->pvalues = (values_or_query_spec_t*)$5;
			$$ = (LITEM)pstmt;
		}
	;

values_or_query_spec:
		VALUES '(' insert_atom_commalist ')'
		{
			sql_declare(values_or_query_spec_t, pvalues);
			pvalues->which = 0;
			pvalues->patoms = $3;
			$$ = (LITEM)pvalues;
		}
	|	query_spec
		{
			sql_declare(values_or_query_spec_t, pvalues);
			pvalues->which = 1;
			pvalues->pquery = (query_spec_t*)$1;
			$$ = (LITEM)pvalues;
		}
	;

insert_atom_commalist:
		insert_atom
		{
			$$ = _sql_create_list();
			_sql_add_tail($$, $1);
		}
	|	insert_atom_commalist ',' insert_atom
		{
			_sql_add_tail($1, $3);
			$$ = $1;
		}
	;

insert_atom:
		atom
		{
			$$ = $1;
		}
	|	NULLX
		{
			$$ = (LITEM)NULL;
		}
	;

open_statement:
		OPEN cursor
	;

rollback_statement:
		ROLLBACK WORK
	;

select_statement:
		SELECT opt_all_distinct selection
		INTO target_commalist
		table_exp
		{
			sql_declare(select_statement_t, pstmt);
			pstmt->alldistinct = $2;
			pstmt->pselection = (selection_t*)$3;
			pstmt->pcommalist = $5;
			pstmt->ptable = (table_exp_t*)$6;
			$$ = (LITEM)pstmt;
		}
	;

opt_all_distinct:
		/* empty */
		{
			$$ = SEL_NOT_SPECIFIED;
		}
	|	ALL
		{
			$$ = SEL_ALL;
		}
	|	DISTINCT
		{
			$$ = SEL_DISTINCT;
		}
	;

update_statement_positioned:
		UPDATE table SET assignment_commalist
		WHERE CURRENT OF cursor
	;

assignment_commalist:
	|	assignment
		{
			$$ = _sql_create_list();
			_sql_add_tail($$, $1);
		}
	|	assignment_commalist ',' assignment
		{
			_sql_add_tail($1, $3);
			$$ = $1;
		}
	;

assignment:
		column EQUALS scalar_exp
		{
			sql_declare(assignment_t, pass);
			pass->pcolumn = (column_t)$1;
			pass->pscalar = (scalar_exp_t*)$3;
			$$ = (LITEM)pass;
		}
	|	column EQUALS NULLX
		{
			sql_declare(assignment_t, pass);
			pass->pcolumn = (column_t)$1;
			pass->pscalar = (scalar_exp_t*)NULL;
			$$ = (LITEM)pass;
		}
	;

update_statement_searched:
		UPDATE table SET assignment_commalist opt_where_clause
		{
			sql_declare(update_statement_searched_t, pupdate);
			pupdate->ptable = (table_t*)$2;
			pupdate->plist = $4;
			pupdate->pwhereclause = (where_clause_t*)$5;
			$$ = (LITEM)pupdate;
		}
	;

target_commalist:
		target
		{
			$$ = _sql_create_list();
			_sql_add_tail($$, $1);
		}
	|	target_commalist ',' target
		{
			_sql_add_tail($1, $3);
			$$ = $1;
		}
	;

target:
		parameter_ref
		{
			$$ = $1;
		}
	;

opt_where_clause:
		/* empty */
		{
			$$ = (LITEM)NULL;
		}
	|	where_clause
		{
			$$ = $1;
		}
	;

	/* query expressions */

sql:	query_spec
		{
			sql_declare(sql_t, psql);
			psql->which = 3;
			psql->pquery = (query_spec_t*)$1;
			$$ = (LITEM)psql;
		}
	;

query_exp:
		query_term
		{
			sql_declare(query_exp_t, pquery);
			pquery->pterm = (query_term_t*)$1;
			$$ = (LITEM)pquery;
		}
	|	query_exp UNION query_term
		{
			sql_declare(query_exp_t, pquery);
			pquery->pterm = (query_term_t*)$3;
			pquery->pexp = (query_exp_t*)$1;
			$$ = (LITEM)pquery;
		}
	|	query_exp UNION ALL query_term
		{
			sql_declare(query_exp_t, pquery);
			pquery->pterm = (query_term_t*)$4;
			pquery->pexp = (query_exp_t*)$1;
			$$ = (LITEM)pquery;
		}
	;

query_term:
		query_spec
		{
			sql_declare(query_term_t, pterm);
			pterm->which = 0;
			pterm->pspec = (query_spec_t*)$1;
			$$ = (LITEM)pterm;
		}
	|	'(' query_exp ')'
		{
			sql_declare(query_term_t, pterm);
			pterm->which = 1;
			pterm->pexp = (void*)$2;
			$$ = (LITEM)pterm;
		}
			
	;

query_spec:
		SELECT opt_all_distinct selection table_exp opt_order_by_clause
		{
			sql_declare(query_spec_t, pspec);
			pspec->alldistinct = $2;
			pspec->pselection = $3;
			pspec->pexp = (table_exp_t*)$4;
			pspec->porderby = $5;
			$$ = (LITEM)pspec;
		}
	;

selection:
		scalar_exp_commalist
		{
			$$ = $1;
		}
	;

table_exp:
		from_clause
		opt_where_clause
		opt_group_by_clause
		opt_having_clause
		{
			sql_declare(table_exp_t, ptable);
			ptable->pfrom = (from_clause_t*)$1;
			ptable->pwhere = (void*)$2;
			ptable->pgroup = $3;
			ptable->phaving = (having_clause_t*)$4;
			$$ = (LITEM)ptable;
		}
	;

from_clause:
		FROM table_ref_commalist
		{
			sql_declare(from_clause_t, pfrom);
			pfrom->which = 0;
			pfrom->plist = $2;
			$$ = (LITEM)pfrom;
		}
	|	FROM table_ref join_ref
		{
			sql_declare(from_clause_t, pfrom);
			pfrom->which = 1;
			pfrom->pjoin = sql_alloc(table_join_t);
			pfrom->pjoin->ptable = (table_ref_t*)$2;
			pfrom->pjoin->pjoin = (join_ref_t*)$3;
			$$ = (LITEM)pfrom;
		}
	;


join_ref:
		INNER join_ref
		{
			join_ref_t *pref = (join_ref_t*)$2;
			pref->type = JOIN_INNER;
			$$ = (LITEM)pref;
		}
	|	LEFT OUTER join_ref
		{
			join_ref_t *pref = (join_ref_t*)$3;
			pref->type = JOIN_LEFT_OUTER;
			$$ = (LITEM)pref;
		}
	|	RIGHT OUTER join_ref
		{
			join_ref_t *pref = (join_ref_t*)$3;
			pref->type = JOIN_RIGHT_OUTER;
			$$ = (LITEM)pref;
		}
	|	JOIN table_ref
		{
			sql_declare(join_ref_t, pref);
			pref->type = JOIN_INNER;
			pref->ptable = (table_ref_t*)$2;
			$$ = (LITEM)pref;
		}
	|	JOIN table_ref join_ref
		{
			sql_declare(join_ref_t, pref);
			pref->type = JOIN_INNER;
			pref->ptable = (table_ref_t*)$2;
			pref->pjoin = (join_ref_t*)$3;
			$$ = (LITEM)pref;
		}
	|	JOIN table_ref ON search_condition
		{
			sql_declare(join_ref_t, pref);
			pref->type = JOIN_INNER;
			pref->ptable = (table_ref_t*)$2;
			pref->psearch = (void*)$4;
			$$ = (LITEM)pref;
		}
	|	JOIN table_ref ON search_condition join_ref
		{
			sql_declare(join_ref_t, pref);
			pref->type = JOIN_INNER;
			pref->ptable = (table_ref_t*)$2;
			pref->psearch = (void*)$4;
			pref->pjoin = (join_ref_t*)$5;
			$$ = (LITEM)pref;
		}
	;

table_ref_commalist:
		table_ref
		{
			$$ = _sql_create_list();
			_sql_add_tail($$, $1);
		}
	|	table_ref_commalist ',' table_ref
		{
			_sql_add_tail($1, $3);
			$$ = $1;
		}
	;

table_ref:
		table
		{
			sql_declare(table_ref_t, pref);
			pref->ptable = (table_t*)$1;
			pref->which = -1;
			$$ = (LITEM)pref;
		}
	|	table NAME
		{
			sql_declare(table_ref_t, pref);
			pref->ptable = (table_t*)$1;
			pref->which = 0;
			pref->alias = (char*)$2;
			$$ = (LITEM)pref;
		}
	|	table AS NAME
		{
			sql_declare(table_ref_t, pref);
			pref->ptable = (table_t*)$1;
			pref->which = 0;
			pref->alias = (char*)$3;
			$$ = (LITEM)pref;
		}
	|	table range_variable
		{
			sql_declare(table_ref_t, pref);
			pref->ptable = (table_t*)$1;
			pref->which = 1;
			pref->prange = (range_variable_t)$2;
			$$ = (LITEM)pref;
		}
	;

where_clause:
		WHERE search_condition
		{
			$$ = $2;
		}
	;

opt_group_by_clause:
		/* empty */
		{
			$$ = (LITEM)NULL;
		}
	|	GROUP BY column_ref_commalist
		{
			$$ = $3;
		}
	;

column_ref_commalist:
		column_ref
		{
			$$ = _sql_create_list();
			_sql_add_tail($$, $1);
		}
	|	column_ref_commalist ',' column_ref
		{
			_sql_add_tail($1, $3);
			$$ = $1;
		}
	;

opt_having_clause:
		/* empty */
		{
			$$ = (LITEM)NULL;
		}
	|	HAVING search_condition
		{
			$$ = $2;
		}
	;

	/* search conditions */

search_condition:
		search_condition OR search_condition
		{
			sql_declare(search_condition_t, psearch);
			psearch->psearch1 = (search_condition_t*)$1;
			psearch->psearch2 = (search_condition_t*)$3;
			psearch->searchop = SEARCHOP_OR;
			$$ = (LITEM)psearch;
		}
	|	search_condition AND search_condition
		{
			sql_declare(search_condition_t, psearch);
			psearch->psearch1 = (search_condition_t*)$1;
			psearch->psearch2 = (search_condition_t*)$3;
			psearch->searchop = SEARCHOP_AND;
			$$ = (LITEM)psearch;
		}
	|	NOT search_condition
		{
			sql_declare(search_condition_t, psearch);
			psearch->psearch2 = (search_condition_t*)$2;
			psearch->searchop = SEARCHOP_NOT;
			$$ = (LITEM)psearch;
		}
	|	'(' search_condition ')'
		{
			sql_declare(search_condition_t, psearch);
			psearch->psearch1 = (search_condition_t*)$2;
			psearch->useparens = 1;
			$$ = (LITEM)psearch;
		}
	|	predicate
		{
			sql_declare(search_condition_t, psearch);
			psearch->ppredicate = (predicate_t*)$1;
			$$ = (LITEM)psearch;
		}
	;

predicate:
		comparison_predicate
		{
			sql_declare(predicate_t, ppred);
			ppred->which = 0;
			ppred->pcomparison = (comparison_predicate_t*)$1;
			$$ = (LITEM)ppred;
		}
	|	in_predicate
		{
			sql_declare(predicate_t, ppred);
			ppred->which = 1;
			ppred->pin = (in_predicate_t*)$1;
			$$ = (LITEM)ppred;
		}
	|	test_for_null
		{
			sql_declare(predicate_t, ppred);
			ppred->which = 2;
			ppred->ptestnull = (test_for_null_t*)$1;
			$$ = (LITEM)ppred;
		}
	|	existence_test
		{
			sql_declare(predicate_t, ppred);
			ppred->which = 3;
			ppred->pexisttest = (existence_test_t*)$1;
			$$ = (LITEM)ppred;
		}
	|	between_predicate
		{
			sql_declare(predicate_t, ppred);
			ppred->which = 4;
			ppred->pbetween = (between_predicate_t*)$1;
			$$ = (LITEM)ppred;
		}
	|	like_predicate
		{
			sql_declare(predicate_t, ppred);
			ppred->which = 5;
			ppred->plike = (like_predicate_t*)$1;
			$$ = (LITEM)ppred;
		}
	|	all_or_any_predicate 
		{
			sql_declare(predicate_t, ppred);
			ppred->which = 6;
			ppred->panyorall = (any_or_all_predicate_t*)$1;
			$$ = (LITEM)ppred;
		}
	;

comparison_predicate:
		scalar_exp comparison scalar_exp
		{
			sql_declare(comparison_predicate_t, ppred);
			ppred->pscalar1 = (scalar_exp_t*)$1;
			ppred->pcomparison = (char*)$2;
			ppred->pscalar2 = (scalar_exp_t*)$3;
			ppred->join_type = JOIN_INNER;
			$$ = (LITEM)ppred;
		}
	|	scalar_exp comparison subquery
		{
			sql_declare(comparison_predicate_t, ppred);
			ppred->pscalar1 = (scalar_exp_t*)$1;
			ppred->pcomparison = (char*)$2;
			ppred->psubquery = (subquery_t*)$3;
			ppred->join_type = JOIN_INNER;
			$$ = (LITEM)ppred;
		}
	;

between_predicate:
		scalar_exp NOT BETWEEN scalar_exp AND scalar_exp
		{
			sql_declare(between_predicate_t, ppred);
			ppred->pscalar1 = (scalar_exp_t*)$1;
			ppred->not = 1;
			ppred->pscalar2 = (scalar_exp_t*)$4;
			ppred->pscalar3 = (scalar_exp_t*)$6;
			$$ = (LITEM)ppred;
		}
	|	scalar_exp BETWEEN scalar_exp AND scalar_exp
		{
			sql_declare(between_predicate_t, ppred);
			ppred->pscalar1 = (scalar_exp_t*)$1;
			ppred->not = 0;
			ppred->pscalar2 = (scalar_exp_t*)$3;
			ppred->pscalar3 = (scalar_exp_t*)$5;
			$$ = (LITEM)ppred;
		}
	;

like_predicate:
		scalar_exp NOT LIKE atom opt_escape
		{
			sql_declare(like_predicate_t, ppred);
			ppred->pscalar = (scalar_exp_t*)$1;
			ppred->not = 1;
			ppred->patom = (atom_t*)$4;
			ppred->pescape = (atom_t*)$5;
			$$ = (LITEM)ppred;
		}
	|	scalar_exp LIKE atom opt_escape
		{
			sql_declare(like_predicate_t, ppred);
			ppred->pscalar = (scalar_exp_t*)$1;
			ppred->not = 0;
			ppred->patom = (atom_t*)$3;
			ppred->pescape = (atom_t*)$4;
			$$ = (LITEM)ppred;
		}
	;

opt_escape:
		/* empty */
		{
			$$ = (LITEM)NULL;
		}
	|	ESCAPE atom
		{
			$$ = $2;
		}
	;

test_for_null:
		column_ref IS NOT NULLX
		{
			sql_declare(test_for_null_t, ptest);
			ptest->pcolumn = (column_ref_t*)$1;
			ptest->isnull = 0;
			$$ = (LITEM)ptest;
		}
	|	column_ref IS NULLX
		{
			sql_declare(test_for_null_t, ptest);
			ptest->pcolumn = (column_ref_t*)$1;
			ptest->isnull = 1;
			$$ = (LITEM)ptest;
		}
	;

in_predicate:
		scalar_exp NOT IN2 subquery
		{
			sql_declare(in_predicate_t, pin);
			pin->pscalar = (scalar_exp_t*)$1;
			pin->not = 1; 
			pin->which = 0;
			pin->psubquery = (subquery_t*)$4;
			$$ = (LITEM)pin;
		}
	|	scalar_exp IN2 subquery
		{
			sql_declare(in_predicate_t, pin);
			pin->pscalar = (scalar_exp_t*)$1;
			pin->not = 0; 
			pin->which = 0;
			pin->psubquery = (subquery_t*)$3;
			$$ = (LITEM)pin;
		}
	|	scalar_exp NOT IN2 '(' atom_commalist ')'
		{
			sql_declare(in_predicate_t, pin);
			pin->pscalar = (scalar_exp_t*)$1;
			pin->not = 1; 
			pin->which = 1;
			pin->patomlist = $5;
			$$ = (LITEM)pin;
		}
	|	scalar_exp IN2 '(' atom_commalist ')'
		{
			sql_declare(in_predicate_t, pin);
			pin->pscalar = (scalar_exp_t*)$1;
			pin->not = 1; 
			pin->which = 1;
			pin->patomlist = $4;
			$$ = (LITEM)pin;
		}
	;

atom_commalist:
		atom
		{
			$$ = _sql_create_list();
			_sql_add_tail($$, $1);
		}
	|	atom_commalist ',' atom
		{
			_sql_add_tail($1, $3);
			$$ = $1;
		}
	;

all_or_any_predicate:
		scalar_exp comparison any_all_some subquery
		{
			sql_declare(any_or_all_predicate_t, pall);
			pall->pscalar = (scalar_exp_t*)$1;
			pall->comparison = (char*)$2;
			pall->anyallsome = $3;
			pall->psubquery = (subquery_t*)$4;
			$$ = (LITEM)pall;
		}
	;
			
any_all_some:
		ANY
		{
			$$ = PRED_ANY;
		}
	|	ALL
		{
			$$ = PRED_ALL;
		}
	|	SOME
		{
			$$ = PRED_SOME;
		}
	;

existence_test:
		EXISTS subquery
		{
			$$ = $2;
		}
	;

subquery:
		'(' query_spec ')'
		{
			$$ = $2;
		}
	;

	/* scalar expressions */

scalar_exp:
		scalar_exp NAME
		{
			char *tmp;
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pscalar1 = (scalar_exp_t*)$1;
			tmp = __sql_alloc(strlen((char*)$2) + 3);
			sprintf(tmp, "%s", (char*)$2);
			pscalar->name = tmp;
			$$ = (LITEM)pscalar;
		}
	|	scalar_exp AS NAME
		{
			char *tmp;
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pscalar1 = (scalar_exp_t*)$1;
			tmp = __sql_alloc(strlen((char*)$3) + 3);
			sprintf(tmp, "%s", (char*)$3);
			pscalar->name = tmp;
			$$ = (LITEM)pscalar;
		}
	|	scalar_exp '+' scalar_exp
		{
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pscalar1 = (scalar_exp_t*)$1;
			pscalar->mathop = MATHOP_PLUS;
			pscalar->pscalar2 = (scalar_exp_t*)$3;
			$$ = (LITEM)pscalar;
		}
	|	scalar_exp '-' scalar_exp
		{
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pscalar1 = (scalar_exp_t*)$1;
			pscalar->mathop = MATHOP_MINUS;
			pscalar->pscalar2 = (scalar_exp_t*)$3;
			$$ = (LITEM)pscalar;
		}
	|	scalar_exp '*' scalar_exp
		{
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pscalar1 = (scalar_exp_t*)$1;
			pscalar->mathop = MATHOP_MULT;
			pscalar->pscalar2 = (scalar_exp_t*)$3;
			$$ = (LITEM)pscalar;
		}
	|	scalar_exp '/' scalar_exp
		{
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pscalar1 = (scalar_exp_t*)$1;
			pscalar->mathop = MATHOP_DIVIDE;
			pscalar->pscalar2 = (scalar_exp_t*)$3;
			$$ = (LITEM)pscalar;
		}
	|	'+' scalar_exp %prec UMINUS
		{
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pscalar1 = (scalar_exp_t*)$2;
			pscalar->mathop = MATHOP_PLUS;
			pscalar->unary = 1;
			$$ = (LITEM)pscalar;
		}
	|	'-' scalar_exp %prec UMINUS
		{
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pscalar1 = (scalar_exp_t*)$2;
			pscalar->mathop = MATHOP_MINUS;
			pscalar->unary = 1;
			$$ = (LITEM)pscalar;
		}
	|	literal
		{
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pliteral = (literal_t*)$1;
			$$ = (LITEM)pscalar;
		}
	|	column_ref
		{
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pcolumnref = (column_ref_t*)$1;
			$$ = (LITEM)pscalar;
		}
	|	function_ref
		{
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pfunction_ref = (function_ref_t*)$1;
			$$ = (LITEM)pscalar;
		}
	|	'(' scalar_exp ')'
		{
			sql_declare(scalar_exp_t, pscalar);
			pscalar->pscalar1 = (scalar_exp_t*)$2;
			$$ = (LITEM)pscalar;
		}
	;
	
scalar_exp_commalist:
		scalar_exp
		{
			$$ = _sql_create_list();
			_sql_add_tail($$, $1);
		}
	|	scalar_exp_commalist ',' scalar_exp
		{
			_sql_add_tail($1, $3);
			$$ = $1;
		}
	;

atom:
		parameter_ref
		{
			sql_declare(atom_t, patom);
			patom->which = 0;
			patom->pparam = (parameter_ref_t*)$1;
			$$ = (LITEM)patom;
		}
	|	literal
		{
			sql_declare(atom_t, patom);
			patom->which = 1;
			patom->pliteral = (literal_t*)$1;
			$$ = (LITEM)patom;
		}
	|	USER
		{
			sql_declare(atom_t, patom);
			patom->which = 2;
			patom->user = 1;
			$$ = (LITEM)patom;
		}
	;

parameter_ref:
		parameter
	|	parameter parameter
	|	parameter INDICATOR parameter
	;

function_ref:
		AMMSC '(' '*' ')'
		{
			sql_declare(function_ref_t, pfunc);
			pfunc->name = (char*)$1;
			pfunc->asterisk = 1;
			$$ = (LITEM)pfunc;
		}
	|	AMMSC '(' DISTINCT column_ref ')'
		{
			sql_declare(function_ref_t, pfunc);
			pfunc->name = (char*)$1;
			pfunc->distinct = 1;
			pfunc->pcolumn = (column_ref_t*)$4;
			$$ = (LITEM)pfunc;
		}
	|	AMMSC '(' column_ref ')'
		{
			sql_declare(function_ref_t, pfunc);
			pfunc->name = (char*)$1;
			pfunc->pcolumn = (column_ref_t*)$3;
			$$ = (LITEM)pfunc;
		}
	|	AMMSC '(' ALL scalar_exp ')'
		{
			sql_declare(function_ref_t, pfunc);
			pfunc->name = (char*)$1;
			pfunc->all = 1;
			pfunc->pscalar = (column_ref_t*)$4;
			$$ = (LITEM)pfunc;
		}
	|	AMMSC '(' scalar_exp ')'
		{
			sql_declare(function_ref_t, pfunc);
			pfunc->name = (char*)$1;
			pfunc->pscalar = (column_ref_t*)$3;
			$$ = (LITEM)pfunc;
		}
	|	AMMSC '(' ')'
		{
			sql_declare(function_ref_t, pfunc);
			pfunc->name = (char*)$1;
			$$ = (LITEM)pfunc;
		}
	;

literal:
		string_literal
		{
			$$ = $1;
		}
	|	INTNUM
		{
			sql_declare(literal_t, plit);
			plit->l = atoi((const char*)$1);
			plit->which = 1;
			$$ = (LITEM)plit;
		}
	|	APPROXNUM
		{
			sql_declare(literal_t, plit);
			plit->d = atof((const char *)$1);
			plit->which = 2;
			$$ = (LITEM)plit;
		}
	|	'*'
		{
			sql_declare(literal_t, plit);
			plit->string = (char*)$1;
			plit->which = 3;
			$$ = (LITEM)plit;
		}
	|	NAME '.' '*'
		{
			sql_declare(literal_t, plit);
			plit->string = (char*)$1;
			plit->which = 4;
			$$ = (LITEM)plit;
		}
	|	DATE_LITERAL
		{
			sql_declare(literal_t, plit);
			plit->string = (char*)$1;
			plit->which = 5;
			$$ = (LITEM)plit;
		}
	|	TIME_LITERAL
		{
			sql_declare(literal_t, plit);
			plit->string = (char*)$1;
			plit->which = 6;
			$$ = (LITEM)plit;
		}
	;

string_literal:

		STRING
		{
			sql_declare(literal_t, plit);
			plit->string = (char*)$1;
			plit->which = 0;
			$$ = (LITEM)plit;
		}
	|
		string_literal STRING
		{
			literal_t *plit = (literal_t*)$1;
			char *tmp = __sql_alloc(strlen((char*)plit->string) + strlen((char*)$2) + 1);
			sprintf(tmp, "%s%s", plit->string, (char*)$2);
			plit->string = tmp;
			$$ = (LITEM)plit;
		}
;
	/* miscellaneous */

table:
		NAME
		{
			sql_declare(table_t, ptable);
			ptable->table = (char*)$1;
			$$ = (LITEM)ptable;
		}
	|	NAME '.' NAME   
		{
			sql_declare(table_t, ptable);
			ptable->owner = (char*)$1;
			ptable->table = (char*)$3;
			$$ = (LITEM)ptable;
		}
	;

trigger_name:
		NAME
		{
			trigger_name_t *pname = sql_alloc(trigger_name_t);
			pname->trigger = (char*)$1;
			$$ = (LITEM)pname;
		}
	|	NAME '.' NAME
		{
			trigger_name_t *pname = sql_alloc(trigger_name_t);
			pname->trigger = (char*)$1;
			pname->owner = (char*)$3;
			$$ = (LITEM)pname;
		}
		;

column_ref:
		NAME
		{
			sql_declare(column_ref_t, pcol);
			pcol->column = (char*)$1;
			$$ = (LITEM)pcol;
		}
	|	NAME NAME
		{
			char *tmp;
			sql_declare(column_ref_t, pcol);
			pcol->column = (char*)$1;
			tmp = __sql_alloc(strlen((char*)$2) + 3);
			sprintf(tmp, "%s", (char*)$2);
			pcol->alias = tmp;
			$$ = (LITEM)pcol;
		}
	|	NAME AS NAME
		{
			char *tmp;
			sql_declare(column_ref_t, pcol);
			pcol->column = (char*)$1;
			tmp = __sql_alloc(strlen((char*)$3) + 3);
			sprintf(tmp, "%s", (char*)$3);
			pcol->alias = tmp;
			$$ = (LITEM)pcol;
		}
	|	NAME '.' NAME
		{
			sql_declare(column_ref_t, pcol);
			pcol->tablename = (char*)$1;
			pcol->column = (char*)$3;
			$$ = (LITEM)pcol;
		}
	|	NAME '.' NAME NAME
		{
			char *tmp;
			sql_declare(column_ref_t, pcol);
			pcol->tablename = (char*)$1;
			pcol->column = (char*)$3;
			tmp = __sql_alloc(strlen((char*)$4) + 3);
			sprintf(tmp, "%s", (char*)$4);
			pcol->alias = tmp;
			$$ = (LITEM)pcol;
		}
	|	NAME '.' NAME AS NAME
		{
			char *tmp;
			sql_declare(column_ref_t, pcol);
			pcol->tablename = (char*)$1;
			pcol->column = (char*)$3;
			tmp = __sql_alloc(strlen((char*)$5) + 3);
			sprintf(tmp, "%s", (char*)$5);
			pcol->alias = tmp;
			$$ = (LITEM)pcol;
		}
	|	NAME '.' NAME '.' NAME    
		{
			sql_declare(column_ref_t, pcol);
			pcol->tablename = (char*)$1;
			pcol->column = (char*)$3;
			pcol->alias = (char*)$5;
			$$ = (LITEM)pcol;
		}
	;

comparison:
		EQUALS 
	|	NOT_EQUALS 
	|	GREATER_THAN 
	|	LESS_THAN 
	|	GREATER_THAN_EQ 
	|	LESS_THAN_EQ
;

data_type:
		CHARACTER
		{
			sql_declare(data_type_t, ptype);
			ptype->type = DT_CHAR;
			ptype->scale = 1;
			$$ = (LITEM)ptype;
		}
	|	CHARACTER '(' INTNUM ')'
		{
			sql_declare(data_type_t, ptype);
			ptype->type = DT_CHAR;
			ptype->scale = atoi((char*)$3);
			$$ = (LITEM)ptype;
		}
	|	NUMERIC
		{
			sql_declare(data_type_t, ptype);
			ptype->type = DT_NUMERIC;
			$$ = (LITEM)ptype;
		}
	|	NUMERIC '(' INTNUM ')'
		{
			sql_declare(data_type_t, ptype);
			ptype->type = DT_NUMERIC;
			ptype->scale = atoi((char*)$3);
			$$ = (LITEM)ptype;
		}
	|	NUMERIC '(' INTNUM ',' INTNUM ')'
		{
			sql_declare(data_type_t, ptype);
			ptype->type = DT_NUMERIC;
			ptype->scale = atoi((char*)$3);
			ptype->precision = atoi((char*)$5);
			$$ = (LITEM)ptype;
		}
	|	DECIMAL2
		{
			sql_declare(data_type_t, ptype);
			ptype->type = DT_DECIMAL;
			$$ = (LITEM)ptype;
		}
	|	DECIMAL2 '(' INTNUM ')'
		{
			sql_declare(data_type_t, ptype);
			ptype->type = DT_DECIMAL;
			ptype->scale = atoi((char*)$3);
			$$ = (LITEM)ptype;
		}
	|	DECIMAL2 '(' INTNUM ',' INTNUM ')'
		{
			sql_declare(data_type_t, ptype);
			ptype->type = DT_DECIMAL;
			ptype->scale = atoi((char*)$3);
			ptype->precision = atoi((char*)$5);
			$$ = (LITEM)ptype;
		}
	|	INTEGER
		{
			sql_declare(data_type_t, ptype);
			ptype->type = DT_INTEGER;
			$$ = (LITEM)ptype;
		}
	|	SMALLINT
		{
			sql_declare(data_type_t, ptype);
			ptype->type = DT_SMALLINT;
			$$ = (LITEM)ptype;
		}
	|	FLOAT2
		{
			sql_declare(data_type_t, ptype);
			ptype->type = DT_FLOAT;
			$$ = (LITEM)ptype;
		}
	|	REAL
		{
			sql_declare(data_type_t, ptype);
			ptype->type = DT_REAL;
			$$ = (LITEM)ptype;
		}
	|	DOUBLE2 PRECISION
		{
			sql_declare(data_type_t, ptype);
			ptype->type = DT_DOUBLE;
			$$ = (LITEM)ptype;
		}
	|	VARCHAR
		{
			sql_declare(data_type_t, ptype);
			ptype->type = DT_VARCHAR;
			ptype->scale = 1;
			$$ = (LITEM)ptype;
		}
	|	VARCHAR '(' INTNUM ')'
		{
			sql_declare(data_type_t, ptype);
			ptype->type = DT_VARCHAR;
			ptype->scale = atoi((char*)$3);
			$$ = (LITEM)ptype;
		}
	;

	/* the various things you can name */

column:		NAME
			{
				$$ = $1;
			}
	;

cursor:		NAME
			{
				$$ = $1;
			}
	;

module:		NAME
			{
				$$ = $1;
			}
	;

parameter:
		':' NAME
			{
				$$ = $2;
			}
	;

procedure:	NAME
			{
				$$ = $1;
			}
	;

range_variable:	NAME
			{
				$$ = $1;
			}
	;

user:		NAME
			{
				$$ = $1;
			}
	;

%%
