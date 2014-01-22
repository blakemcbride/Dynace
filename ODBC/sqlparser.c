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

#ifdef _MSC_VER
#if _MSC_VER > 1200
#define _CRT_SECURE_NO_DEPRECATE
#define _POSIX_
#endif
#endif


#include <windows.h>
#include <stdio.h>
#include "sqlparse.h"
#include "sqlstructs.h"

static 	CRITICAL_SECTION cs;
static int IsCritInit = 0;

extern int _cdecl yyparse(void *YYPARSE_PARAM);
extern void * _cdecl yy_scan_string(const char *p);
extern void yy_delete_buffer(void *);

typedef struct 
{
	SQLSTR sqlstr;
	int iDBMS;
	const char *stmt_separator;
} SQLContext;

// conversion to minimal standard SQL
static void add_table_ref(table_ref_commalist_t plist, table_ref_t *ptable);
static void convert_joins(table_exp_t *pexp);

// generation of sql strings
static void build_output(sql_list_t root, SQLContext *pcontext);
static void build_sql(sql_t *psql, SQLContext *pcontext);
static void build_query_spec(query_spec_t *pquery, SQLContext *pcontext);
static void build_all_distinct(int alldistinct, SQLContext *pcontext);
static void build_scalar_exp_commalist(scalar_exp_commalist_t psel, SQLContext *pcontext);
static void build_table_exp(table_exp_t *pexp, SQLContext *pcontext);
static void build_ordering_spec_commalist(ordering_spec_commalist_t porder, SQLContext *pcontext);
static void build_literal(literal_t *pliteral, SQLContext *pcontext);
static void build_column_ref(column_ref_t *pref, SQLContext *pcontext);
static void build_function_ref(function_ref_t* pref, SQLContext *pcontext);
static void build_scalar_exp(scalar_exp_t *pscalar, SQLContext *pcontext);
static void build_mathop(int mathop, SQLContext *pcontext);
static void	build_from_clause(from_clause_t* pfrom, SQLContext *pcontext);
static void build_search_condition(search_condition_t *pwhere, SQLContext *pcontext);
static void build_group_by_clause(group_by_clause_t pgroup, SQLContext *pcontext);
static void build_having_clause(having_clause_t *phaving, SQLContext *pcontext);
static void build_table_join(table_join_t *pjoin, SQLContext *pcontext);
static void build_join_ref(join_ref_t *pjoin, SQLContext *pcontext);
static void build_table_commalist(table_ref_commalist_t plist, SQLContext *pcontext);
static void build_table_ref(table_ref_t *ptable, SQLContext *pcontext);
static void build_table(table_t *ptable, SQLContext *pcontext);
static void build_range_variable(range_variable_t prange, SQLContext *pcontext);
static void build_predicate(predicate_t *ppred, SQLContext *pcontext);
static void build_comparison_pred(comparison_predicate_t *pcomparison, SQLContext *pcontext);
static void build_in_pred(in_predicate_t* pin, SQLContext *pcontext);
static void build_test_for_null(test_for_null_t *ptestnull, SQLContext *pcontext);
static void build_existence_test(existence_test_t *pexisttest, SQLContext *pcontext);
static void build_between_pred(between_predicate_t *pbetween, SQLContext *pcontext);
static void build_like_pred(like_predicate_t *plike, SQLContext *pcontext);
static void build_any_or_all_pred(any_or_all_predicate_t *panyorall, SQLContext *pcontext);
static void build_atom_commalist(atom_commalist_t patomlist, SQLContext *pcontext);
static void build_manipulative_statement(manipulative_statement_t *pstmt, SQLContext *pcontext);
static void build_atom(atom_t *patom, SQLContext *pcontext);
static void build_insert_atom_commalist(insert_atom_commalist_t list, SQLContext *pcontext);
static void build_values_or_query_spec(values_or_query_spec_t *pspec, SQLContext *pcontext);
static void build_column_commalist(column_commalist_t plist, SQLContext *pcontext);
static void build_insert_statement(insert_statement_t *pinsert, SQLContext *pcontext);
static void build_delete_statement(delete_statement_searched_t *pdel, SQLContext *pcontext);
static void build_update_statement(update_statement_searched_t *pup, SQLContext *pcontext);
static void build_assignment_commalist(assignment_commalist_t plist, SQLContext *pcontext);
static void build_assignment(assignment_t *pass, SQLContext *pcontext);
static void build_subquery(subquery_t *psubquery, SQLContext *pcontext);
static void build_column_ref_commalist(column_ref_commalist_t plist, SQLContext *pcontext);
static void build_schema(schema_t *pschema, SQLContext *pcontext);
static void build_module_def(module_def_t *pdef, SQLContext *pcontext);
static void build_schema_element(schema_element_t *pschema, SQLContext *pcontext);
static void build_drop_trigger(drop_trigger_t *pdroptrigger, SQLContext *pcontext);
static void build_trigger_name(trigger_name_t *pname, SQLContext *pcontext);
static void build_drop_table(drop_table_t *pdrop, SQLContext *pcontext);
static void build_create_table(base_table_def_t *pdef, SQLContext *pcontext);
static void build_base_table_element(base_table_element_t* pelement, SQLContext *pcontext);
static void build_column_def(column_def_t *pdef, SQLContext *pcontext);
static void build_datatype(data_type_t *ptype, SQLContext *pcontext);
static void build_column_def_opt_list(column_def_opt_list_t plist, SQLContext *pcontext);
static void build_table_constraint_def(table_constraint_def_t *pdef, SQLContext *pcontext);

char *TranslateToOracle(const char *pSql)
{
	sql_list_t root;
	SQLContext sqlctx;
	char *ret = NULL;

	memset(&sqlctx, 0, sizeof(sqlctx));
	sqlctx.iDBMS = DBMS_ORACLE;
	sqlctx.stmt_separator = ";";

	// I know. I know. This isn't thread safe. But hopefully the first query will be
	// executed before any other threads get started.
	if (IsCritInit == 0)
	{
		IsCritInit = 1;
		InitializeCriticalSection(&cs);
	}
	
	EnterCriticalSection(&cs);

	// if it is a create anything query or a grant query
	if (strncmp(pSql, "create ", strlen("create ")) && strncmp(pSql, "grant ", 6))
	{
		void *pBuffer = yy_scan_string(pSql);
		if (!yyparse(&root))
		{
			build_output(root, &sqlctx);
		}

		yy_delete_buffer(pBuffer);

		if (sqlctx.sqlstr.size)
		{
			ret = malloc(sqlctx.sqlstr.size);
			strcpy(ret, sqlctx.sqlstr.buf);
		}
		else
			ret = strdup(pSql);
	}
	else
	{
		ret = malloc(strlen(pSql) + 1);
		strcpy(ret, pSql);
	}

	__free_allocated_blocks();

	LeaveCriticalSection(&cs);
	return ret;	
}

static scalar_exp_t *convert_one_selection(scalar_exp_t *pexp, table_ref_t *ptable)
{
	scalar_exp_t *pnewentry;
	const char *tablename;

	if (ptable->which == 0 && ptable->alias)
		tablename = ptable->alias;
	else
		tablename = ptable->ptable->table;

	pnewentry = __sql_alloc(sizeof(scalar_exp_t));
	pnewentry->pliteral = __sql_alloc(sizeof(literal_t));
	pnewentry->pliteral->string = __sql_alloc(strlen(tablename) + 1);
	pnewentry->pliteral->which = 4;
	strcpy(pnewentry->pliteral->string, tablename);

	return pnewentry;
}

static void convert_select_list(selection_t pselection, from_clause_t *pfrom)
{
	LITEM item, item2;

	if (!pselection|| !pfrom)
		return;

	for (item = _sql_get_first(pselection); item; item = _sql_get_next(item))
	{
		scalar_exp_t *pexp = (scalar_exp_t*)_sql_get_element(item);
		if (pexp->pliteral && pexp->pliteral->which == 3)
		{
			if (pfrom->which == 0)
			{
				for (item2 = _sql_get_first(pfrom->plist); item2; item2 = _sql_get_next(item2))
				{
					table_ref_t *ptable = (table_ref_t*)_sql_get_element(item2);
					scalar_exp_t *pnewentry = convert_one_selection(pexp, ptable);
					_sql_add_tail(pselection, (int)pnewentry);
				}
			}
			else
			{
				table_join_t *pjoin = pfrom->pjoin;
				table_ref_t *ptable = pjoin->ptable;
				scalar_exp_t *pnewentry = convert_one_selection(pexp, ptable);
				join_ref_t *pjoinref = pjoin->pjoin;

				_sql_add_tail(pselection, (int)pnewentry);
				while (pjoinref)
				{
					pnewentry = convert_one_selection(pexp, pjoinref->ptable);
					_sql_add_tail(pselection, (int)pnewentry);
					pjoinref = pjoinref->pjoin;
				}
			}
			
			_sql_remove(pselection, item);
		}

	}
}


static void add_table_ref(table_ref_commalist_t plist, table_ref_t *ptable)
{
	LITEM item;

	if (!ptable || !plist)
		return;

	for (item = _sql_get_first(plist); item; item = _sql_get_next(item))
	{
		table_ref_t *tmpTable = (table_ref_t*)_sql_get_element(item);
		if (!stricmp(tmpTable->ptable->table, ptable->ptable->table) && 
			tmpTable->which == ptable->which)
		{
			if (tmpTable->which == 0 && tmpTable->alias && ptable->alias && 
				!stricmp(tmpTable->alias, ptable->alias))
				return;
			else 
				continue;
		}
	}

	_sql_add_tail(plist, (LITEM)ptable);
}

static int contains_table(table_ref_t *ptable, scalar_exp_t *pexp)
{
	if (!ptable || !pexp)
		return 0;

	if (pexp->pcolumnref && ptable->ptable)
	{
		if (pexp->pcolumnref->tablename && ptable->ptable->table &&
			!stricmp(pexp->pcolumnref->tablename, ptable->ptable->table))
			return 1;
		if (pexp->pcolumnref->tablename && ptable->alias && 
			!stricmp(pexp->pcolumnref->tablename, ptable->alias))
			return 1;
	}

	if (contains_table(ptable, pexp->pscalar1))
		return 1;
	else
		return contains_table(ptable, pexp->pscalar2);
}

static void set_predicate(search_condition_t *psearch, table_ref_t *ptable, int join_type)
{
	if (!psearch)
		return;

	if (psearch->ppredicate && psearch->ppredicate->which == 0)
	{
		psearch->ppredicate->pcomparison->join_type = join_type;
		if (contains_table(ptable, psearch->ppredicate->pcomparison->pscalar1))
		{
			scalar_exp_t* ptmp = psearch->ppredicate->pcomparison->pscalar1;
			psearch->ppredicate->pcomparison->pscalar1 = psearch->ppredicate->pcomparison->pscalar2;
			psearch->ppredicate->pcomparison->pscalar2 = ptmp;
		}
	}

	set_predicate(psearch->psearch1, ptable, join_type);
	set_predicate(psearch->psearch2, ptable, join_type);
}

static void convert_joins(table_exp_t *pexp)
{
	where_clause_t *pwhere = pexp->pwhere;
	table_join_t *ptjoin = pexp->pfrom->pjoin;
	join_ref_t *pjoinref;
	search_condition_t *psearch;

	if (!ptjoin || pexp->pfrom->which != 1)
		return;

	pexp->pfrom->plist = _sql_create_list();
	pexp->pfrom->which = 0;
	add_table_ref(pexp->pfrom->plist, ptjoin->ptable);

	for (pjoinref = ptjoin->pjoin; pjoinref; pjoinref = pjoinref->pjoin)
	{
		// add table to from clause
		add_table_ref(pexp->pfrom->plist, pjoinref->ptable);

		// make sure joins are the correct type
		psearch = pjoinref->psearch;
		set_predicate(psearch, pjoinref->ptable, pjoinref->type);

		// add join to the where clause
		pwhere = __sql_alloc(sizeof(search_condition_t));
		pwhere->psearch1 = pjoinref->psearch;
		pwhere->psearch2 = pexp->pwhere;

		// since the original where clause could have an OR in it
		// (e.g. where x = 1 or x = 2), we must put parenthesis around
		// the original where clause. If we do not then
		// select * from table1 join table2 on table1.x = table2.y where table1.x = 5 or table1.x = 6
		// would translate to 
		// select * from table1, table2 where table1.x = table2.x and table1.x = 5 or table2.x = 6
		// This query would fetch all rows where table1.x = 6 regardless if tabel1.x = table2.x or not.
		// Adding the parens produces this.
		// select * from table1, table2 where table1.x = table2.x and (table1.x = 5 or table2.x = 6)
		// Which is what is really intended.

		if (pwhere->psearch2)
			pwhere->psearch2->useparens = 1;

		pwhere->searchop = SEARCHOP_AND;
		pexp->pwhere = pwhere;
	}

}

static void build_output(sql_list_t root, SQLContext *pcontext)
{
	LITEM item;
	int use_seperator = 0;

	if (!root)
		return;

	for (item = _sql_get_first(root); item; item = _sql_get_next(item))
	{
		build_sql((sql_t*)_sql_get_element(item), pcontext);

		if (use_seperator)
			sql_append(pcontext->sqlstr, pcontext->stmt_separator);

		use_seperator = 1;
	}
}

static void build_sql(sql_t* psql, SQLContext *pcontext)
{
	if (!psql)
		return;

	switch(psql->which)
	{
	case 0: //schema_t
		build_schema(psql->pschema, pcontext);
		break;
	case 1: //module_def_t
		build_module_def(psql->pmodule, pcontext);
		break;
	case 2: //manipulative_statement_t
		build_manipulative_statement(psql->pstmt, pcontext);
		break;
	case 3: //query_spec_t
		build_query_spec(psql->pquery, pcontext);
		break;
	case 4: //schema_element_t
		build_schema_element(psql->pschema_element, pcontext);
		break;
	}
}

static void convert_orderby(ordering_spec_commalist_t porder, from_clause_t *pfrom)
{
	LITEM item;

	if (pfrom->which != 0 || _sql_get_count(pfrom->plist) != 1 || !porder)
		return;

	for (item = _sql_get_first(porder); item; item = _sql_get_next(item))
	{
		ordering_spec_t *pspec = (ordering_spec_t*)_sql_get_element(item);
		if (pspec->pcolumn && pspec->pcolumn->tablename == NULL && pspec->pcolumn->owner == NULL)
		{
			table_ref_t *ptable = (table_ref_t*)_sql_get_element(_sql_get_first(pfrom->plist));
			pspec->pcolumn->tablename = (ptable->which == 0 && ptable->alias ?
						     ptable->alias : ptable->ptable->table);
			pspec->pcolumn->owner = ptable->ptable->owner;
		}
	}
}

static void build_query_spec(query_spec_t *pquery, SQLContext *pcontext)
{
	if (!pquery)
		return;

//	not necessary for Oracle 9
//	if (pcontext->iDBMS == DBMS_ORACLE)
//		convert_joins(pquery->pexp);

	sql_append(pcontext->sqlstr, "select ");
	build_all_distinct(pquery->alldistinct, pcontext);
	sql_append(pcontext->sqlstr, " ");

	if (pcontext->iDBMS == DBMS_ORACLE)
		convert_select_list(pquery->pselection, pquery->pexp->pfrom);

	build_scalar_exp_commalist(pquery->pselection, pcontext);
	sql_append(pcontext->sqlstr, " ");
	build_table_exp(pquery->pexp, pcontext);
	sql_append(pcontext->sqlstr, " ");
	convert_orderby(pquery->porderby, pquery->pexp->pfrom);
	build_ordering_spec_commalist(pquery->porderby, pcontext);
	sql_append(pcontext->sqlstr, " ");
}

static void build_all_distinct(int alldistinct, SQLContext *pcontext)
{
	switch(alldistinct)
	{
	case SEL_ALL:
		sql_append(pcontext->sqlstr, "all");
		break;
	case SEL_DISTINCT:
		sql_append(pcontext->sqlstr, "distinct");
		break;
	}
}

static void build_scalar_exp_commalist(scalar_exp_commalist_t psel, SQLContext *pcontext)
{
	LITEM item;
	int useComma = 0;

	for (item = _sql_get_first(psel); item; item = _sql_get_next(item))
	{
		scalar_exp_t *pdata;
		if (useComma)
			sql_append(pcontext->sqlstr, ", ")

		pdata = (scalar_exp_t*)_sql_get_element(item);
		build_scalar_exp(pdata, pcontext);

		useComma = 1;
	}

}

static void build_table_exp(table_exp_t *pexp, SQLContext *pcontext)
{
	if (pexp->pfrom)
		build_from_clause(pexp->pfrom, pcontext);

	sql_append(pcontext->sqlstr, " ");

	if (pexp->pwhere)
	{
		sql_append(pcontext->sqlstr, "where ");
		build_search_condition(pexp->pwhere, pcontext);
	}

	sql_append(pcontext->sqlstr, " ");

	if (pexp->pgroup)
		build_group_by_clause(pexp->pgroup, pcontext);

	sql_append(pcontext->sqlstr, " ");

	if (pexp->phaving)
		build_having_clause(pexp->phaving, pcontext);
}

static void build_ordering_spec_commalist(ordering_spec_commalist_t porder, SQLContext *pcontext)
{
	LITEM item;
	int useComma = 0;
	ordering_spec_t *pspec;

	if (!porder)
		return;

	sql_append(pcontext->sqlstr, "order by ");

	for (item = _sql_get_first(porder); item; item = _sql_get_next(item))
	{
		if (useComma)
			sql_append(pcontext->sqlstr, ", ");

		pspec = (ordering_spec_t*)_sql_get_element(item);
		//The column reference in an ORDER BY could either be a named reference or an ordinal.
		//If the pcolumn field is set, expect a reference (a *column_ref_t); otherwise, assume 
		//it must be a column number.
		if (pspec->pcolumn)
			build_column_ref(pspec->pcolumn, pcontext);
		else
			sql_append(pcontext->sqlstr, pspec->colnum);
		switch (pspec->ascdesc)
		{
		case ORDER_ASC:
			sql_append(pcontext->sqlstr, " asc");
			break;
		case ORDER_DESC:
			sql_append(pcontext->sqlstr, " desc");
			break;
		}

		useComma = 1;
	}
}

static void build_literal(literal_t *pliteral, SQLContext *pcontext)
{
	char tmpBuf[32];

	switch(pliteral->which)
	{
	case 0:
		sql_append(pcontext->sqlstr, pliteral->string);
		break;
	case 1:
		sprintf(tmpBuf, "%ld", pliteral->l);
		sql_append(pcontext->sqlstr, tmpBuf);
		break;
	case 2:
		sprintf(tmpBuf, "%f", pliteral->d);
		sql_append(pcontext->sqlstr, tmpBuf);
		break;
	case 3:
		sql_append(pcontext->sqlstr, pliteral->string);
		break;
	case 4:
		sql_append(pcontext->sqlstr, pliteral->string);
		sql_append(pcontext->sqlstr, ".*");
		break;
	case 5:
		sql_append(pcontext->sqlstr, "TO_DATE(");
		sql_append(pcontext->sqlstr, pliteral->string);
		sql_append(pcontext->sqlstr, ", 'YYYY-MM-DD')");
		break;
	case 6:
		sql_append(pcontext->sqlstr, "TO_DATE(");
		sql_append(pcontext->sqlstr, pliteral->string);
		sql_append(pcontext->sqlstr, ", 'YYYY-MM-DD HH24:MI:SS')");
		break;
	}
}

static void build_column_ref(column_ref_t *pref, SQLContext *pcontext)
{
	if (pref->owner)
	{
		sql_append(pcontext->sqlstr, pref->owner);
		sql_append(pcontext->sqlstr, ".");
	}

	if (pref->tablename)
	{
		sql_append(pcontext->sqlstr, pref->tablename);
		sql_append(pcontext->sqlstr, ".");
	}

	if (pref->column)
	{
		sql_append(pcontext->sqlstr, pref->column);
	}

	if (pref->alias)
	{
		sql_append(pcontext->sqlstr, " as ");
		sql_append(pcontext->sqlstr, pref->alias);
	}
}

static void build_function_ref(function_ref_t* pref, SQLContext *pcontext)
{
	sql_append(pcontext->sqlstr, pref->name);
	if (!stricmp(pref->name, "sysdate"))
		return;

	sql_append(pcontext->sqlstr, "(");
	if (pref->distinct)
		sql_append(pcontext->sqlstr, "distinct ");
	if (pref->asterisk)
		sql_append(pcontext->sqlstr, "*");
	if (pref->all)
		sql_append(pcontext->sqlstr, "all ");
	if (pref->pcolumn)
		build_column_ref(pref->pcolumn, pcontext);
	if (pref->pscalar)
		build_scalar_exp(pref->pscalar, pcontext);
	sql_append(pcontext->sqlstr, ")");
}

static void build_scalar_exp(scalar_exp_t *pdata, SQLContext *pcontext)
{
	if (pdata->useparens)
		sql_append(pcontext->sqlstr, "(");

	if (pdata->pscalar1)
	{
		if (pdata->mathop && pdata->unary)
		{
			build_mathop(pdata->mathop, pcontext);
		}
		else
		{
			build_scalar_exp(pdata->pscalar1, pcontext);
			sql_append(pcontext->sqlstr, " ");
			if (pdata->name)
			{
				sql_append(pcontext->sqlstr, pdata->name);
				sql_append(pcontext->sqlstr, " ");
			}
			else if (pdata->mathop && pdata->pscalar2)
			{
				build_mathop(pdata->mathop, pcontext);
				sql_append(pcontext->sqlstr, " ");
				build_scalar_exp(pdata->pscalar2, pcontext);
			}

		}
	}
	else if (pdata->pliteral)
	{
		build_literal(pdata->pliteral, pcontext);
	}
	else if (pdata->pcolumnref)
	{
		build_column_ref(pdata->pcolumnref, pcontext);
	}
	else if (pdata->pfunction_ref)
	{
		build_function_ref(pdata->pfunction_ref, pcontext);
	}

	if (pdata->useparens)
		sql_append(pcontext->sqlstr, ")");
}

static void build_mathop(int mathop, SQLContext *pcontext)
{
	switch(mathop)
	{
	case MATHOP_PLUS:
		sql_append(pcontext->sqlstr, "+");
		break;
	case MATHOP_MINUS:
		sql_append(pcontext->sqlstr, "-");
		break;
	case MATHOP_MULT:
		sql_append(pcontext->sqlstr, "*");
		break;
	case MATHOP_DIVIDE:
		sql_append(pcontext->sqlstr, "/");
		break;
	}
}

static void	build_from_clause(from_clause_t* pfrom, SQLContext *pcontext)
{
	sql_append(pcontext->sqlstr, "from ");

	switch(pfrom->which)
	{
	case 0:
		build_table_commalist(pfrom->plist, pcontext);
		break;
	case 1:
		build_table_join(pfrom->pjoin, pcontext);
		break;
	}
}

static void build_search_condition(search_condition_t *pwhere, SQLContext *pcontext)
{
	if (pwhere->searchop == SEARCHOP_NOT)
		sql_append(pcontext->sqlstr, " not ");

	if (pwhere->ppredicate)
	{
		build_predicate(pwhere->ppredicate, pcontext);
		return;
	}

	if (pwhere->useparens)
		sql_append(pcontext->sqlstr, "(");

	if (pwhere->psearch1)
		build_search_condition(pwhere->psearch1, pcontext);

	if (pwhere->psearch2)
	{
		switch (pwhere->searchop)
		{
		case SEARCHOP_AND:
			sql_append(pcontext->sqlstr, " and ");
			break;
		case SEARCHOP_OR:
			sql_append(pcontext->sqlstr, " or ");
			break;
		}

		sql_append(pcontext->sqlstr, " ");
		build_search_condition(pwhere->psearch2, pcontext);
	}

	if (pwhere->useparens)
		sql_append(pcontext->sqlstr, ") ");
}

static void build_group_by_clause(group_by_clause_t pgroup, SQLContext *pcontext)
{
	if (!pgroup)
		return;

	sql_append(pcontext->sqlstr, "group by ");
	build_column_ref_commalist(pgroup, pcontext);
}

static void build_having_clause(having_clause_t *phaving, SQLContext *pcontext)
{
	if (!phaving)
		return;

	sql_append(pcontext->sqlstr, "having ");
	build_search_condition(phaving, pcontext);
}

static void build_table_join(table_join_t *pjoin, SQLContext *pcontext)
{
	if (!pjoin)
		return;
		
	build_table_ref(pjoin->ptable, pcontext);
	build_join_ref(pjoin->pjoin, pcontext);
}

static void build_join_ref(join_ref_t *pjoin, SQLContext *pcontext)
{
	if (!pjoin)
		return;
		
	switch (pjoin->type)
	{
		case JOIN_INNER:
			sql_append(pcontext->sqlstr, " JOIN ");
			build_table_ref(pjoin->ptable, pcontext);
			
			if (pjoin->psearch)
			{
				sql_append(pcontext->sqlstr, " ON ");
				build_search_condition(pjoin->psearch, pcontext);
			}
			
			if (pjoin->pjoin)
				build_join_ref(pjoin->pjoin, pcontext);

			break;
		case JOIN_LEFT_OUTER:
			sql_append(pcontext->sqlstr, " LEFT OUTER JOIN ");
			build_table_ref(pjoin->ptable, pcontext);
			
			if (pjoin->psearch)
			{
				sql_append(pcontext->sqlstr, " ON ");
				build_search_condition(pjoin->psearch, pcontext);
			}
			
			if (pjoin->pjoin)
				build_join_ref(pjoin->pjoin, pcontext);

			break;			
		case JOIN_RIGHT_OUTER:
			sql_append(pcontext->sqlstr, " RIGHT OUTER JOIN ");
			build_table_ref(pjoin->ptable, pcontext);
			
			if (pjoin->psearch)
			{
				sql_append(pcontext->sqlstr, " ON ");
				build_search_condition(pjoin->psearch, pcontext);
			}
			
			if (pjoin->pjoin)
				build_join_ref(pjoin->pjoin, pcontext);

			break;			
	}
}

static void build_table_commalist(table_ref_commalist_t plist, SQLContext *pcontext)
{
	LITEM item;
	int useComma = 0;
	table_ref_t *ptable;

	for (item = _sql_get_first(plist); item; item = _sql_get_next(item))
	{
		if (useComma)
			sql_append(pcontext->sqlstr, ", ");

		ptable = (table_ref_t*)_sql_get_element(item);
		build_table_ref(ptable, pcontext);
		useComma = 1;
	}
}

static void build_table_ref(table_ref_t *ptable, SQLContext *pcontext)
{
	build_table(ptable->ptable, pcontext);
	switch(ptable->which)
	{
	case 0:
		sql_append(pcontext->sqlstr, " ");
		sql_append(pcontext->sqlstr, ptable->alias);
		break;
	case 1:
		sql_append(pcontext->sqlstr, " ");
		build_range_variable(ptable->prange, pcontext);
		break;
	}
}

static void build_table(table_t *ptable, SQLContext *pcontext)
{
	if (ptable->owner)
	{
		sql_append(pcontext->sqlstr, ptable->owner);
		sql_append(pcontext->sqlstr, ".");
	}

	sql_append(pcontext->sqlstr, ptable->table);
}

static void build_range_variable(range_variable_t prange, SQLContext *pcontext)
{
	sql_append(pcontext->sqlstr, prange);
}

static void build_predicate(predicate_t *ppred, SQLContext *pcontext)
{
	switch(ppred->which)
	{
	case 0:
		build_comparison_pred(ppred->pcomparison, pcontext);
		break;
	case 1:
		build_in_pred(ppred->pin, pcontext);
		break;
	case 2:
		build_test_for_null(ppred->ptestnull, pcontext);
		break;
	case 3:
		build_existence_test(ppred->pexisttest, pcontext);
		break;
	case 4:
		build_between_pred(ppred->pbetween, pcontext);
		break;
	case 5:
		build_like_pred(ppred->plike, pcontext);
		break;
	case 6:
		build_any_or_all_pred(ppred->panyorall, pcontext);
		break;
	}
}

static void build_comparison_pred(comparison_predicate_t *pcomparison, SQLContext *pcontext)
{
	build_scalar_exp(pcomparison->pscalar1, pcontext);

	if (pcomparison->join_type == JOIN_RIGHT_OUTER)
		sql_append(pcontext->sqlstr, " (+) ")
	else
		sql_append(pcontext->sqlstr, " ")

	sql_append(pcontext->sqlstr, pcomparison->pcomparison);
	
	if (pcomparison->pscalar2)
	{
		sql_append(pcontext->sqlstr, " ");
		build_scalar_exp(pcomparison->pscalar2, pcontext);
	}

	if (pcomparison->psubquery)
	{
		sql_append(pcontext->sqlstr, " (");
		build_query_spec(pcomparison->psubquery, pcontext);
		sql_append(pcontext->sqlstr, ")");
	}

	if (pcomparison->join_type == JOIN_LEFT_OUTER)
		sql_append(pcontext->sqlstr, " (+)");

}

static void build_in_pred(in_predicate_t* pin, SQLContext *pcontext)
{
	build_scalar_exp(pin->pscalar, pcontext);
	if (pin->not)
		sql_append(pcontext->sqlstr, " not");

	sql_append(pcontext->sqlstr, " in (");
	if (pin->which == 0)
		build_query_spec(pin->psubquery, pcontext);
	else
		build_atom_commalist(pin->patomlist, pcontext);

	sql_append(pcontext->sqlstr, ")");
}

static void build_test_for_null(test_for_null_t *ptestnull, SQLContext *pcontext)
{
	build_column_ref(ptestnull->pcolumn, pcontext);
	if (ptestnull->isnull)
		sql_append(pcontext->sqlstr, " is null")
	else
		sql_append(pcontext->sqlstr, " is not null")
}

static void build_existence_test(existence_test_t *pexisttest, SQLContext *pcontext)
{
	sql_append(pcontext->sqlstr, "exists ");
	build_subquery(pexisttest, pcontext);
}

static void build_between_pred(between_predicate_t *pbetween, SQLContext *pcontext)
{
	build_scalar_exp(pbetween->pscalar1, pcontext);
	if (pbetween->not)
		sql_append(pcontext->sqlstr, " not");
	sql_append(pcontext->sqlstr, " between ");
	build_scalar_exp(pbetween->pscalar2, pcontext);
	sql_append(pcontext->sqlstr, " and ");
	build_scalar_exp(pbetween->pscalar3, pcontext);
}

static void build_like_pred(like_predicate_t *plike, SQLContext *pcontext)
{
	build_scalar_exp(plike->pscalar, pcontext);
	if (plike->not)
		sql_append(pcontext->sqlstr, " not");
	sql_append(pcontext->sqlstr, " like ");
	build_atom(plike->patom, pcontext);
	if (plike->pescape)
	{
		sql_append(pcontext->sqlstr, " escape ");
		build_atom(plike->pescape, pcontext);
	}
}

static void build_any_or_all_pred(any_or_all_predicate_t *panyorall, SQLContext *pcontext)
{
	build_scalar_exp(panyorall->pscalar, pcontext);
	sql_append(pcontext->sqlstr, " ");
	sql_append(pcontext->sqlstr, panyorall->comparison);
	switch(panyorall->anyallsome)
	{
	case PRED_ANY:
		sql_append(pcontext->sqlstr, " any ");
		break;
	case PRED_ALL:
		sql_append(pcontext->sqlstr, " all ");
		break;
	case PRED_SOME:
		sql_append(pcontext->sqlstr, " some ");
		break;
	}
	build_subquery(panyorall->psubquery, pcontext);
}

static void build_atom_commalist(atom_commalist_t patomlist, SQLContext *pcontext)
{
	LITEM item;
	int usecomma = 0;

	for (item = _sql_get_first(patomlist); item; item = _sql_get_next(item))
	{
		atom_t *patom;
		
		if (usecomma)
			sql_append(pcontext->sqlstr, ", ");

		patom = (atom_t*)_sql_get_element(item);
		build_atom(patom, pcontext);
	}
}

static void build_manipulative_statement(manipulative_statement_t *pstmt, SQLContext *pcontext)
{
	switch(pstmt->which)
	{
	case 0:
		// delete_statement_searched
		build_delete_statement(pstmt->pdelsearched, pcontext);
		break;
	case 1:
		// insert statement
		build_insert_statement(pstmt->pinsert, pcontext);
		break;
	case 3:
		// update where
		build_update_statement(pstmt->pupdatesearched, pcontext);
		break;
	}
}

static void build_insert_statement(insert_statement_t *pinsert, SQLContext *pcontext)
{
	sql_append(pcontext->sqlstr, "insert into ");
	build_table(pinsert->ptable, pcontext);
	sql_append(pcontext->sqlstr, " ");

	if (pinsert->pcommalist)
	{
		sql_append(pcontext->sqlstr, "(");
		build_column_commalist(pinsert->pcommalist, pcontext);
		sql_append(pcontext->sqlstr, ") ");
	}

	build_values_or_query_spec(pinsert->pvalues, pcontext);
}

static void build_column_commalist(column_commalist_t plist, SQLContext *pcontext)
{
	LITEM item;
	int usecomma = 0;

	for (item = _sql_get_first(plist); item; item = _sql_get_next(item))
	{
		column_t pcolumn;
		
		if (usecomma)
			sql_append(pcontext->sqlstr, ", ");

		pcolumn = (column_t)_sql_get_element(item);
		sql_append(pcontext->sqlstr, pcolumn);
		usecomma = 1;
	}
}

static void build_values_or_query_spec(values_or_query_spec_t *pspec, SQLContext *pcontext)
{
	if (pspec->which == 0)
	{
		sql_append(pcontext->sqlstr, " values (");
		build_insert_atom_commalist(pspec->patoms, pcontext);
		sql_append(pcontext->sqlstr, ")");
	}
	else if (pspec->which == 1)
		build_query_spec(pspec->pquery, pcontext);
}

static void build_insert_atom_commalist(insert_atom_commalist_t list, SQLContext *pcontext)
{
	LITEM item;
	int usecomma = 0;

	for (item = _sql_get_first(list); item; item = _sql_get_next(item))
	{
		atom_t *patom = (atom_t*)_sql_get_element(item);
		if (usecomma)
			sql_append(pcontext->sqlstr, ", ");
		
		//SQL NULL is stored as a null atom_t*
		if (patom)
			build_atom(patom, pcontext);
		else
			sql_append(pcontext->sqlstr, "null");
		
		usecomma = 1;
	}
}

static void build_atom(atom_t *patom, SQLContext *pcontext)
{
	switch(patom->which)
	{
	case 0:
		//parameter_ref
		break;
	case 1:
		// literal
		build_literal(patom->pliteral, pcontext);
		break;
	case 2:
		sql_append(pcontext->sqlstr, "user");
		break;
	}
}

static void build_delete_statement(delete_statement_searched_t *pdel, SQLContext *pcontext)
{
	sql_append(pcontext->sqlstr, "delete from ");
	build_table(pdel->ptable, pcontext);
	if (pdel->pwhere)
	{
		sql_append(pcontext->sqlstr, " where ");
		build_search_condition(pdel->pwhere, pcontext);
	}
}

static void build_update_statement(update_statement_searched_t *pup, SQLContext *pcontext)
{
	sql_append(pcontext->sqlstr, "update ");
	build_table(pup->ptable, pcontext);
	sql_append(pcontext->sqlstr, " set ");
	build_assignment_commalist(pup->plist, pcontext);
	if (pup->pwhereclause)
	{
		sql_append(pcontext->sqlstr, " where ");
		build_search_condition(pup->pwhereclause, pcontext);
	}
}

static void build_assignment_commalist(assignment_commalist_t plist, SQLContext *pcontext)
{
	LITEM item;
	int usecomma = 0;

	for (item = _sql_get_first(plist); item; item = _sql_get_next(item))
	{
		assignment_t *pass;
		
		if (usecomma)
			sql_append(pcontext->sqlstr, ", ");
		pass = (assignment_t*)_sql_get_element(item);
		build_assignment(pass, pcontext);
		usecomma = 1;
	}
}

static void build_assignment(assignment_t *pass, SQLContext *pcontext)
{
	sql_append(pcontext->sqlstr, pass->pcolumn);
	sql_append(pcontext->sqlstr, " = ");
	if (pass->pscalar)
		build_scalar_exp(pass->pscalar, pcontext);
	else
		sql_append(pcontext->sqlstr, "NULL");
}

static void build_subquery(subquery_t *psubquery, SQLContext *pcontext)
{
	sql_append(pcontext->sqlstr, "(");
	build_query_spec(psubquery, pcontext);
	sql_append(pcontext->sqlstr, ")");
}

static void build_column_ref_commalist(column_ref_commalist_t plist, SQLContext *pcontext)
{
	LITEM item;
	int usecomma = 0;

	for (item = _sql_get_first(plist); item; item = _sql_get_next(item))
	{
		column_ref_t *pref;
		
		if (usecomma)
			sql_append(pcontext->sqlstr, ", ");
		pref = (column_ref_t*)_sql_get_element(item);
		build_column_ref(pref, pcontext);
		usecomma = 1;
	}
}

static void build_schema(schema_t *pschema, SQLContext *pcontext)
{
}

static void build_module_def(module_def_t *pdef, SQLContext *pcontext)
{
}

static void build_schema_element(schema_element_t *pschema, SQLContext *pcontext)
{
	switch(pschema->which)
	{
	case 0: // create table
		build_create_table(pschema->ptabledef, pcontext);
		break;
	case 3: // drop trigger
		build_drop_trigger(pschema->pdroptrigger, pcontext);
		break;
	case 4: // drop table
		build_drop_table(pschema->pdroptable, pcontext);
		break;
	}
}

static void build_drop_trigger(drop_trigger_t *pdroptrigger, SQLContext *pcontext)
{
	sql_append(pcontext->sqlstr, "drop trigger ");
	build_trigger_name(pdroptrigger, pcontext);
}

static void build_trigger_name(trigger_name_t *pname, SQLContext *pcontext)
{
	if (pname->owner)
	{
		sql_append(pcontext->sqlstr, pname->owner);
		sql_append(pcontext->sqlstr, ".");
	}

	sql_append(pcontext->sqlstr, pname->trigger);
}

static void build_drop_table(drop_table_t *pdrop, SQLContext *pcontext)
{
	sql_append(pcontext->sqlstr, "drop table ");
	build_table(pdrop, pcontext);
}

static void build_create_table(base_table_def_t *pdef, SQLContext *pcontext)
{
	LITEM item;
	int useComma = 0;

	sql_append(pcontext->sqlstr, "create table ");
	build_table(pdef->ptable, pcontext);
	sql_append(pcontext->sqlstr, " (");

	for (item = _sql_get_first(pdef->plist); item; item = _sql_get_next(item))
	{
		base_table_element_t *pelement;
		
		if (useComma)
			sql_append(pcontext->sqlstr, ", ");

		pelement = (base_table_element_t*)_sql_get_element(item);
		build_base_table_element(pelement, pcontext);

		useComma = 1;
	}

	sql_append(pcontext->sqlstr, ")");
}

static void build_base_table_element(base_table_element_t* pelement, SQLContext *pcontext)
{
	switch(pelement->which)
	{
	case 0:
		build_column_def(pelement->pcolumn, pcontext);
		break;
	case 1:
		build_table_constraint_def(pelement->pconstraint, pcontext);
		break;
	}
}

static void build_column_def(column_def_t *pdef, SQLContext *pcontext)
{
	sql_append(pcontext->sqlstr, pdef->pcolumn);
	sql_append(pcontext->sqlstr, " ");
	build_datatype(pdef->ptype, pcontext);
	sql_append(pcontext->sqlstr, " ");
	build_column_def_opt_list(pdef->popt, pcontext);
}

static void build_datatype(data_type_t *ptype, SQLContext *pcontext)
{
	char buf[64];

	switch (ptype->type)
	{
	case DT_CHAR:
		sprintf(buf, "char(%d)", ptype->scale);
		break;
	case DT_NUMERIC:
		sprintf(buf, "number(%d, %d)", ptype->scale, ptype->precision);
		break;
	case DT_DECIMAL:
		sprintf(buf, "decimal(%d, %d)", ptype->scale, ptype->precision);
		break;
	case DT_INTEGER:
		strcpy(buf, "number(10)");
		break;
	case DT_SMALLINT:
		strcpy(buf, "number(5)");
		break;
	case DT_FLOAT:
		strcpy(buf, "float");
		break;
	case DT_REAL:
		strcpy(buf, "real");
		break;
	case DT_DOUBLE:
		strcpy(buf, "double precision");
		break;
	case DT_VARCHAR:
		sprintf(buf, "varchar2(%d)", ptype->scale);
		break;
	}

	sql_append(pcontext->sqlstr, buf);
}

static void build_column_def_opt_list(column_def_opt_list_t plist, SQLContext *pcontext)
{
	LITEM item;

	if (!plist)
		return;
	
	for (item = _sql_get_first(plist); item; item = _sql_get_next(item));
	{
		column_def_opt_t *popt = (column_def_opt_t*)_sql_get_element(item);
		if (popt->notnull)
			sql_append(pcontext->sqlstr, " not null");
	}
}

static void build_table_constraint_def(table_constraint_def_t *pdef, SQLContext *pcontext)
{
	switch(pdef->which)
	{
	case 0: //UNIQUE
		sql_append(pcontext->sqlstr, "unique (");
		build_column_commalist(pdef->punique, pcontext);
		sql_append(pcontext->sqlstr, ")");
		break;
	case 1: //primary key
		sql_append(pcontext->sqlstr, "primary key (");
		build_column_commalist(pdef->pprimarykeys, pcontext);
		sql_append(pcontext->sqlstr, ")");
		break;
	}
}
