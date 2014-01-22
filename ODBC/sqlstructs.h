/*
  Copyright (c) 1996 Blake McBride
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

  1. Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#pragma once

#include <stdlib.h>

#define sql_alloc(s) ((s*)__sql_alloc(sizeof(s)))
#define sql_declare(type, name) type *name = sql_alloc(type)

extern void *  __sql_alloc(size_t size);
extern void  __free_allocated_blocks(void);

typedef struct
{
	char *buf;
	unsigned size;
} SQLSTR, *PSQLSTR;

#define sql_append(sqlstr, str) \
{\
	while (sqlstr.buf == NULL || strlen(sqlstr.buf) + strlen(str) >= sqlstr.size)\
	{\
		char *ptmp = sqlstr.buf;\
		sqlstr.size += 512;\
		sqlstr.buf = (char*)__sql_alloc(sqlstr.size);\
		if (ptmp != NULL)\
		{\
			strcpy(sqlstr.buf, ptmp);\
		}\
	}\
	strcat(sqlstr.buf, str);\
}

typedef int LLIST;
typedef int LITEM;

typedef struct sql_list_element_s
{
	struct sql_list_element_s *pnext;
	struct sql_list_element_s *pprev;
	int pdata;
} sql_list_element;

typedef struct 
{
	sql_list_element *pfirst;
	sql_list_element *plast;
	int count;
} sql_list;


#define _sql_create_list() ((LLIST)__sql_alloc(sizeof(sql_list)))
#define _sql_add_tail(list, data) \
{ \
	sql_list *__plist = (sql_list*)list; \
	sql_list_element *__pelement = __sql_alloc(sizeof(sql_list_element)); \
	__pelement->pdata = data;\
	if (__plist->plast) \
	{ \
		__pelement->pprev = __plist->plast;\
		__plist->plast->pnext = __pelement; \
		__plist->plast = __pelement; \
	} \
	else \
	{ \
		__plist->pfirst = __plist->plast = __pelement;\
	}\
\
	__plist->count += 1;\
}

#define _sql_remove(list, item)\
{\
	sql_list *plist = (sql_list*)list;\
	sql_list_element *pelement = (sql_list_element*)item;\
\
	if (pelement->pprev)\
		pelement->pprev->pnext = pelement->pnext;\
	else\
		plist->pfirst = pelement->pnext;\
\
	if (pelement->pnext)\
		pelement->pnext->pprev = pelement->pprev;\
	else\
		plist->plast = pelement->pprev;\
\
	plist->count -= 1;\
}


#define _sql_get_count(list) ((sql_list*)list)->count
#define _sql_get_first(list) ((LITEM) ((sql_list *)list)->pfirst)
#define _sql_get_element(item) (((sql_list_element*)item)->pdata)
#define _sql_get_next(item) ((LITEM) ((sql_list_element *)item)->pnext)

#define IS_NULLX				1
#define NOT_NULLX				2
#define NOT_NULLX_UNIQUE		3
#define NOT_NULLX_PKEY			4

#define OP_SELECT				1
#define OP_DELETE				2
#define OP_INSERT				3
#define OP_UPDATE				4
#define OP_REFERENCES			5

#define LANG_COBOL				1
#define LANG_FORTRAN			2
#define LANG_PASCAL2			3
#define LANG_PLI				4
#define LANG_C					5
#define LANG_ADA				6

#define ORDER_NOT_SPECIFIED		0
#define ORDER_DESC				1
#define ORDER_ASC				2

#define SEL_NOT_SPECIFIED		0
#define SEL_ALL					1
#define SEL_DISTINCT			2

#define JOIN_INNER				1
#define JOIN_LEFT_OUTER			2
#define JOIN_RIGHT_OUTER		3

#define SEARCHOP_AND			1
#define SEARCHOP_OR				2
#define SEARCHOP_NOT			3

#define PRED_ANY				1
#define PRED_ALL				2
#define PRED_SOME				3

#define MATHOP_NONE				0
#define MATHOP_PLUS				1
#define MATHOP_MINUS			2
#define MATHOP_MULT				3
#define MATHOP_DIVIDE			4

#define UNARY_NOTSPECIFIED		0
#define UNARY_PLUS				1
#define UNARY_MINUS				2

#define DT_CHAR					1
#define DT_NUMERIC				2
#define DT_DECIMAL				3
#define DT_INTEGER				4
#define DT_SMALLINT				5
#define DT_FLOAT				6
#define DT_REAL					7
#define DT_DOUBLE				8
#define DT_VARCHAR				9

typedef char * cursor_t;
typedef char * module_t;
typedef char * parameter_t;
typedef char * procedure_t;
typedef char * range_variable_t;
typedef char * user_t;
typedef char * column_t;

typedef struct
{
	int type;
	int precision;
	int scale;
} data_type_t;

// if owner is valid then tablename must also be valid
typedef struct
{
	char *column;
	char *alias;
	char *tablename;
	char *owner;
} column_ref_t;

typedef struct
{
	char *table;
	char *owner;
} table_t;

typedef struct
{
	char *trigger;
	char *owner;
} trigger_name_t;

typedef trigger_name_t drop_trigger_t;

typedef struct
{
	union
	{
		char *string;
		long l;
		double d;
	};

	// 0 means string is valid and it is NOT *
	// 1 means l is valid
	// 2 means d is valid
	// 3 means string is valid and it is *
	// 4 means string is valid and it is the table name to qualifiy an *
	int which;
} literal_t;

/*
	name is always valid.
	only the following combinations of other vars are valid
	asterisk || (distinct && pcolumn) || (all && pscalar)
*/
typedef struct
{
	char *name;
	int distinct;
	int asterisk;
	int all;
	column_ref_t *pcolumn;
//	scalar_exp_t *pscalar;
	void *pscalar; // recursion workaround
} function_ref_t;

typedef struct scalar_exp_s
{
	struct scalar_exp_s *pscalar1;
	char *name;
	int mathop;
	struct scalar_exp_s *pscalar2;
	int unary;
	literal_t *pliteral;
	column_ref_t *pcolumnref;
	function_ref_t *pfunction_ref;
	int useparens;
} scalar_exp_t;

typedef struct
{
	parameter_t *pparam1;
	parameter_t *pparam2;
	char *indicator;
} parameter_ref_t;

typedef struct
{
	union
	{
		parameter_ref_t *pparam;
		literal_t *pliteral;
		int user;
	};
	int which;
} atom_t;

typedef LLIST scalar_exp_commalist_t;

typedef scalar_exp_commalist_t selection_t;

typedef struct
{
	table_t *ptable;
	union
	{
		char *alias;
		range_variable_t prange;
	};
	int which;
} table_ref_t;


typedef LLIST table_ref_commalist_t;

// this structure does not follow the pattern in that it combines all
// possible elements without a union. This is for simplification.
typedef struct join_ref_s
{
	int type;
	struct join_ref_s *pjoin;
	table_ref_t *ptable;
//	search_condition_t *psearch;
	void *psearch; // recursion workaround
} join_ref_t;

typedef struct
{
	table_ref_t *ptable;
	join_ref_t *pjoin;
} table_join_t;

typedef struct
{
	union
	{
		table_ref_commalist_t plist;
		table_join_t *pjoin;
	};
	int which;
} from_clause_t;

typedef LLIST column_ref_commalist_t;

typedef column_ref_commalist_t group_by_clause_t;

typedef struct
{
	from_clause_t *pfrom;
//	where_clause_t *pwhere;
	void *pwhere; // recursion workaround
	group_by_clause_t pgroup;
//	having_clause_t phaving;
	void *phaving; // recursion workaround
} table_exp_t;

typedef int asc_desc_t;

typedef struct
{
	char* colnum;          // NULL means column_ref_t
	column_ref_t *pcolumn; // NULL means INTNUM (colnum must be set)
	asc_desc_t ascdesc;
} ordering_spec_t;

typedef LLIST ordering_spec_commalist_t;

/*
	if name is not NULL, only pscalar1 is valid.
	if mathop is not MATHOP_NONE then only pscalar1 and pscalar2 are valid
	if unary is not UNARY_NOTSPECIFIED then only pscalar1 is valid
	if pliteral is not NULL then only it is valid
	if pcolumnref is not null then only it is valid
	if pfunctionref is not null then only it is valid
	if useparens is non zero then only it and pscalar1 is valid
*/
typedef struct
{
	int alldistinct;
	selection_t pselection;
	table_exp_t *pexp;
	ordering_spec_commalist_t porderby;
} query_spec_t;

typedef query_spec_t subquery_t;

typedef struct
{
	scalar_exp_t *pscalar1;
	char *pcomparison;
	scalar_exp_t *pscalar2;
	subquery_t *psubquery;
	int join_type;
} comparison_predicate_t;

typedef LLIST atom_commalist_t;

typedef struct
{
	scalar_exp_t *pscalar;
	int not;
	union
	{
		subquery_t *psubquery;
		atom_commalist_t patomlist;
	};
	int which;
} in_predicate_t;

typedef struct
{
	column_ref_t *pcolumn;
	int isnull;
} test_for_null_t;

typedef struct
{
	scalar_exp_t *pscalar;
	atom_t *patom;
	atom_t *pescape;
	int not;
} like_predicate_t;

typedef struct
{
	scalar_exp_t *pscalar1;
	scalar_exp_t *pscalar2;
	scalar_exp_t *pscalar3;
	int not;
} between_predicate_t;

typedef subquery_t existence_test_t;

typedef struct
{
	scalar_exp_t *pscalar;
	char *comparison;
	int anyallsome;
	subquery_t *psubquery;
} any_or_all_predicate_t;

typedef struct
{
	union
	{
		comparison_predicate_t *pcomparison;
		in_predicate_t *pin;
		test_for_null_t *ptestnull;
		existence_test_t *pexisttest;
		between_predicate_t *pbetween;
		like_predicate_t *plike;
		any_or_all_predicate_t *panyorall;
	};
	int which;
} predicate_t;

typedef struct search_condition_s
{
	struct search_condition_s *psearch1;
	struct search_condition_s *psearch2;
	int searchop;
	int useparens;
	predicate_t *ppredicate;
} search_condition_t;

typedef search_condition_t having_clause_t;

typedef search_condition_t where_clause_t;

typedef struct
{
	union
	{
		query_spec_t *pspec;
//		query_exp_t *pexp;
		void *pexp; // recursion workaround
	};
	int which;
} query_term_t;

typedef struct query_exp_s
{
	query_term_t *pterm;
	struct query_exp_s *pexp;
} query_exp_t;

typedef parameter_ref_t target_t;

typedef LLIST target_commalist_t;

typedef struct
{
	column_t pcolumn;
	scalar_exp_t *pscalar; // NULL is NULLX
} assignment_t;

typedef LLIST assignment_commalist_t;

typedef struct 
{
	table_t *ptable;
	assignment_commalist_t plist;
	where_clause_t *pwhereclause;
} update_statement_searched_t;

typedef struct
{
	table_t *ptable;
	assignment_commalist_t passignment;
	cursor_t cursor;
} update_statement_positioned_t;

typedef struct
{
	int alldistinct;
	selection_t *pselection;
	target_commalist_t pcommalist;
	table_exp_t *ptable;
} select_statement_t;

typedef int rollback_statement_t; // no data

typedef cursor_t open_statement_t;

typedef atom_t insert_atom_t; // NULL is NULLX

typedef LLIST insert_atom_commalist_t;

typedef struct
{
	union
	{
		insert_atom_commalist_t patoms;
		query_spec_t *pquery;
	};
	int which;
} values_or_query_spec_t;

typedef LLIST column_commalist_t;

typedef struct
{
	table_t *ptable;
	column_commalist_t pcommalist;
	values_or_query_spec_t *pvalues;
} insert_statement_t;

typedef struct
{
	cursor_t *pcursor;
	target_commalist_t plist;
} fetch_statement_t;

typedef struct
{
	table_t *ptable;
	cursor_t *pcursor;
} delete_statement_positioned_t;

typedef struct 
{
	table_t *ptable;
	where_clause_t *pwhere;
} delete_statement_searched_t;

typedef int commit_statement_t;  // no data

typedef cursor_t close_statement_t;

typedef struct
{
	union
	{
		delete_statement_searched_t *pdelsearched;
		insert_statement_t *pinsert;
		select_statement_t *pselect;
		update_statement_searched_t *pupdatesearched;
		close_statement_t *pclose;
		commit_statement_t *pcommit;
		delete_statement_positioned_t *pdeletepositioned;
		fetch_statement_t *pfetch;
		open_statement_t *popen;
		rollback_statement_t *prollback;
		update_statement_positioned_t *pupdatepositioned;
	};
	int which;
} manipulative_statement_t;

typedef struct 
{
	parameter_t *pparam;
	data_type_t *ptype;
} parameter_def_t;

typedef LLIST parameter_def_list_t;

typedef LLIST manipulative_statement_list_t;

typedef struct
{
	procedure_t *pprocedure;
	parameter_def_list_t pparams;
	manipulative_statement_list_t plist;
} procedure_def_t;

typedef LLIST procedure_def_list_t;

typedef struct
{
	cursor_t *pcursor;
	query_exp_t *pquery;
	ordering_spec_commalist_t porderby;
} cursor_def_t;

typedef LLIST cursor_def_list_t;

typedef int lang_t;

typedef struct
{
	module_t *pmodule;
	lang_t *plang;
	user_t *puser;
	cursor_def_list_t pcursordef;
	procedure_def_list_t pproceduredef;
} module_def_t;

typedef user_t grantee_t; // NULL means PUBLIC

typedef LLIST grantee_commalist_t;

typedef struct
{
	int type;
	column_commalist_t pcolumns;
} operation_t;

typedef LLIST operation_commalist_t;

typedef struct
{
	union
	{
		int all;
		operation_commalist_t pops;
	};
	int which;
} privileges_t;

typedef struct
{
	privileges_t *pprivs;
	table_t *ptable;
	grantee_commalist_t pgrantees;
	int withgrant;
} privilege_def_t;

typedef struct
{
	table_t *ptable;
	column_commalist_t pcolumns;
	query_spec_t *pquery;
	int withcheck;
} view_def_t;

typedef struct
{
	table_t *ptable;
	column_commalist_t pcolumns;
} references_t;

typedef struct
{
	column_commalist_t pcolumns;
	references_t *preferences;
} foreign_key_t;

typedef struct
{
	union
	{
		column_commalist_t punique;
		column_commalist_t pprimarykeys;
		foreign_key_t *pforeignkey;
		search_condition_t *pcheck;
	};
	int which;
} table_constraint_def_t;

typedef struct
{
	union
	{
		literal_t *pliteral;
		int nullx;
		char *puser;
	};
	int which;
} default_t;
	
typedef struct
{
	union
	{
		int notnull;
		default_t *pdefault;
		search_condition_t *psearch;
		references_t *preferences;
	};
	int which;
} column_def_opt_t;

typedef LLIST column_def_opt_list_t;

typedef struct 
{
	column_t pcolumn;
	data_type_t *ptype;
	column_def_opt_list_t popt;
} column_def_t;

typedef struct 
{
	union
	{
		column_def_t *pcolumn;
		table_constraint_def_t *pconstraint;
	};
	int which;
} base_table_element_t;

typedef LLIST base_table_element_commalist_t;

typedef struct
{
	table_t *ptable;
	base_table_element_commalist_t plist;
} base_table_def_t;

typedef table_t drop_table_t;

typedef struct
{
	union
	{
		base_table_def_t *ptabledef;
		view_def_t *pviewdef;
		privilege_def_t *pprivilegedef;
		drop_trigger_t *pdroptrigger;
		drop_table_t *pdroptable;
	};
	int which;
} schema_element_t;

typedef LLIST schema_element_list_t;

typedef struct
{
	user_t *puser;
	schema_element_list_t plist;
} schema_t;

typedef struct
{
	union
	{
		schema_t *pschema;
		module_def_t *pmodule;
		manipulative_statement_t *pstmt;
		query_spec_t *pquery;
		schema_element_t *pschema_element;
	};
	int which;
} sql_t;

typedef LLIST sql_list_t;
