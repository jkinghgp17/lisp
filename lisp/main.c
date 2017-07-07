/*** includes ***/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "mpc.h"

/* if compiling on Windows compile these functions */
#ifdef _WIN32
#include <string.h>

/*** defines ***/

#define BUFSIZE 2048
static char buffer[BUFSIZE];

/* fake readline function */
char *readline(char *prompt) {
	fputs(prompt, stdout);
	fgets(buffer, BUFSIZE, stdin);
	char *cpy = malloc(strlen(buffer) + 1);
	strcpy(cpy, buffer);
	cpy[strlen(cpy) - 1] = '\0';
	return cpy;
}

/* fake add_history function */
void add_history(char *unused) {}

/* Otherwise include the editline headers */
#else
#include <editline/readline.h>
#include <editline/history.h>
#endif

/* macro for error checking */
#define LASSERT(args, cond, err) \
	if (!(cond)) { lval_del(args); return lval_err(err); }

/* enum of possible lisp value types */
enum {
	LVAL_NUM,
	LVAL_ERR,
	LVAL_SYM,
	LVAL_SEXPR,
	LVAL_QEXPR
};

/* enum of possible error types */
enum {
	LERR_DIV_ZERO, 
	LERR_BAD_OP,
	LERR_BAD_NUM
};

/* declare new lisp value struct */
typedef struct lval {
	int type;
	long num;
	char* err;
	char* sym;
	/* count and pointer to a list of "lval*" */
	int count;
	struct lval** cell;
} lval;

/*** function templates ***/

void lval_expr_print(lval* v, char open, char close);
void lval_print(lval* v);
lval* lval_pop(lval* v, int i);
lval* lval_take(lval* v, int i);
lval* lval_eval_sexpr(lval* v);
lval* lval_eval(lval* v);

/*** lval ***/

/* lval Constructor */
lval* lval_num(long x) {
	lval* v = malloc(sizeof(lval));
	v->type = LVAL_NUM;
	v->num = x;
	return v;
}

/* lval Constructor */
lval* lval_err(char* m) {
	lval* v = malloc(sizeof(lval));
	v->type = LVAL_ERR;
	v->err = malloc(strlen(m) + 1);
	strcpy(v->err, m);
	return v;
}

/* lval Constructor */
lval* lval_sym(char* m) {
	lval* v = malloc(sizeof(lval));
	v->type = LVAL_SYM;
	v->sym = malloc(strlen(m) + 1);
	strcpy(v->sym, m);
	return v;
}

/* lval Constructor */
lval* lval_sexpr(void) {
	lval* v = malloc(sizeof(lval));
	v->type = LVAL_SEXPR;
	v->count = 0;
	v->cell = NULL;
	return v;
}

/* lval Constructor */
lval* lval_qexpr(void) {
	lval* v = malloc(sizeof(lval));
	v->type = LVAL_QEXPR;
	v->count = 0;
	v->cell = NULL;
	return v;
}

/* lval Deconstructor */
void lval_del(lval* v) {
	switch (v->type) {
		/* Do nothing for number type */
		case LVAL_NUM: break;

			       /* For err or sym free the string */
		case LVAL_ERR: free(v->err); break;
		case LVAL_SYM: free(v->sym); break;

			       /* For sexpr delete all elements inside */
		case LVAL_QEXPR:
		case LVAL_SEXPR:
			       for (int i = 0; i < v->count; i++) {
				       lval_del(v->cell[i]);
			       }
			       /* Also free the memory alloc to contain pointers */
			       free(v->cell);
			       break;
	}

	free(v);
}

lval* lval_read_num(mpc_ast_t* t) {
	errno = 0;
	long x = strtol(t->contents, NULL, 10);
	return errno != ERANGE ? lval_num(x) : lval_err("invalid number");
}

lval* lval_add(lval* v, lval* x) {
	v->count++;
	v->cell = realloc(v->cell, sizeof(lval*) * v->count);
	v->cell[v->count - 1] = x;
	return v;
}

lval* lval_read(mpc_ast_t* t) {
	/* If symbol or number return conversion to that type */
	if (strstr(t->tag, "number")) { return lval_read_num(t); }
	if (strstr(t->tag, "symbol")) { return lval_sym(t->contents); }

	/* If root (>) or sexpr then create empty list */
	lval* x = NULL;
	if (strcmp(t->tag, ">") == 0) { x = lval_sexpr(); }
	if (strstr(t->tag, "sexpr")) { x = lval_sexpr(); }
	if (strstr(t->tag, "qexpr")) { x = lval_qexpr(); }

	/* Fill this list with any valid expression contained within */
	for (int i = 0; i < t->children_num; i++) {
		if (strcmp(t->children[i]->contents, "(") == 0) { continue; }
		if (strcmp(t->children[i]->contents, ")") == 0) { continue; }
		if (strcmp(t->children[i]->contents, "{") == 0) { continue; }
		if (strcmp(t->children[i]->contents, "}") == 0) { continue; }
		if (strcmp(t->children[i]->tag, "regex") == 0) { continue; }
		x = lval_add(x, lval_read(t->children[i]));
	}

	return x;
}

void lval_print(lval* v) {
	switch(v->type) {
		case LVAL_NUM: printf("%li", v->num); break;
		case LVAL_ERR: printf("Error: %s", v->err); break;
		case LVAL_SYM: printf("%s", v->sym); break;
		case LVAL_SEXPR: lval_expr_print(v, '(', ')'); break;
		case LVAL_QEXPR: lval_expr_print(v, '{', '}'); break;
	}
}

/* print entire lval expr */
void lval_expr_print(lval* v, char open, char close) {
	putchar(open);
	for (int i = 0; i < v->count; i++) {
		/* print value contained within */
		lval_print(v->cell[i]);

		/* do not print trailing space if last element */
		if (i != (v->count-1)) {
			putchar(' ');
		}
	}
	putchar(close);
}

/* print an lval followed by a newline */
void lval_println(lval* v) {
	lval_print(v);
	putchar('\n');
}

int number_of_nodes(mpc_ast_t* t) {
	if (t->children_num == 0) { return 1; }
	if (t->children_num >= 1) {
		int total = 1;
		for (int i = 0; i < t->children_num; i++) {
			total += number_of_nodes(t->children[i]);
		}
		return total;
	}
	return 0;
}

lval* builtin_op(lval* a, char* op) {
	/* Ensure all arguments are numbers */
	for (int i = 0; i < a->count; i++) {
		if (a->cell[i]->type != LVAL_NUM) {
			lval_del(a);
			return lval_err("Cannot operate on non-number!");
		}
	}

	/* pop first elements */
	lval* x = lval_pop(a, 0);

	/* if no arguments and sub then perform unary negation */
	if ((strcmp(op, "-") == 0) && a->count == 0) {
		x->num = -x->num;
	}

	/* while there are still elements remaining */
	while (a->count > 0) {
		/* pop the next element */
		lval* y = lval_pop(a, 0);

		if (strcmp(op, "+") == 0) { x->num += y->num; }
		if (strcmp(op, "-") == 0) { x->num -= y->num; }
		if (strcmp(op, "*") == 0) { x->num *= y->num; }
		if (strcmp(op, "/") == 0) {
			if (y->num == 0) {
				lval_del(x); lval_del(y);
				x = lval_err("Division By Zero!"); break;
			}
			x->num /= y->num;
		}

		lval_del(y);
	}
	lval_del(a);
	return x;
}

/* head function: takes q-expr and returns first element */
lval* builtin_head(lval* a) {
	LASSERT(a, a->count == 1, "Function 'head' passed too many arguments");
	LASSERT(a, a->cell[0]->type == LVAL_QEXPR, "Function 'head' passed incorrect type");
	LASSERT(a, a->cell[0]->count == 0, "Function 'head' passed {}");

	/* Otherwise take first argument */
	lval* v = lval_take(a, 0);

	/* Delete all elements that are not head and return */
	while (v->count > 1) { lval_del(lval_pop(v, 1)); }
	return v;
}

lval* builtin_tail(lval* a) {
	/* Check Error Conditions */
	LASSERT(a, a->count == 1, "Function 'tail' passed too many arguments");
	LASSERT(a, a->cell[0]->type == LVAL_QEXPR, "Function 'tail' passed incorrect type");
	LASSERT(a, a->cell[0]->count == 0, "Function 'tail' passed {}");

	/* Otherwise take first argument */
	lval* v = lval_take(a, 0);

	/*Delete first element and return */
	lval_del(lval_pop(v, 0));
	return v;
}

/* takes in s-expr and returns q-expr */
lval* builtin_list(lval* a) {
	a->type = LVAL_QEXPR;
	return a;
}

/* takes in a q-expr and return evaluated s-expr */
lval* builtin_eval(lval* a) {
	LASSERT(a, a->count == 1, "Function 'eval' passed too many arguments");
	LASSERT(a, a->cell[0]->type == LVAL_QEXPR, "Function 'eval' passed incorrect type");

	lval* x = lval_take(a, 0);
	x->type = LVAL_SEXPR;
	return lval_eval(x);
}

lval* lval_join(lval* x, lval* y) {
	/* For each cell in 'y' add it to 'x' */
	while (y->count) {
		x = lval_add(x, lval_pop(y, 0));
	}

	/* Delete the empty 'y' and return 'x' */
	lval_del(y);
	return x;
}

/* joins multiple q-expr's together */
lval* builtin_join(lval* a) {
	for (int i = 0; i < a->count; i++) {
		LASSERT(a, a->cell[i]->type == LVAL_QEXPR, "Function 'join' passed incorrect type");
	}

	lval* x = lval_pop(a, 0);

	while (a->count) {
		x = lval_join(x, lval_pop(a, 0));
	}

	lval_del(a);
	return x;
}

/* builtin function lookup */
lval* builtin(lval* a, char* func) {
	if (strcmp("list", func) == 0) { return builtin_list(a); }
	if (strcmp("head", func) == 0) { return builtin_head(a); }
	if (strcmp("tail", func) == 0) { return builtin_tail(a); }
	if (strcmp("join", func) == 0) { return builtin_join(a); }
	if (strcmp("eval", func) == 0) { return builtin_eval(a); }
	if (strstr("+-/*", func)) { return builtin_op(a, func); }
	lval_del(a);
	return lval_err("Unknown function");
}

/* pop element at index i off the sexpr */
lval* lval_pop(lval* v, int i) {
	/* find the item at i */
	lval* x = v->cell[i];

	/* shift memory after the item at i over the top */
	memmove(&v->cell[i], &v->cell[i+1], sizeof(lval*) * (v->count-i-1));

	/* decrease the count of items in the list */
	v->count--;

	/* reallocate the memor used */
	v->cell = realloc(v->cell, sizeof(lval*) * v->count);
	return x;
}

lval* lval_take(lval* v, int i) {
	lval* x = lval_pop(v, i);
	lval_del(v);
	return x;
}

lval* lval_eval(lval* v) {
	/* evaluate sexpressions */
	if (v->type == LVAL_SEXPR) { return lval_eval_sexpr(v); }
	/* all other lval types remain the same */
	return v;
}

lval* lval_eval_sexpr(lval* v) {
	/* Evaluate children */
	for (int i = 0; i < v->count; i++) {
		v->cell[i] = lval_eval(v->cell[i]);
	}

	/* Error Checking */
	for (int i = 0; i < v->count; i++) {
		if (v->cell[i]->type == LVAL_ERR) { return lval_take(v, i); }
	}

	/* empty expression */
	if (v->count == 0) { return v; }

	/* single expression */
	if (v->count == 1) { return lval_take(v, 0); }

	/* ensure first element is symbol */
	lval* f = lval_pop(v, 0);
	if (f->type != LVAL_SYM) {
		lval_del(f);
		lval_del(v);
		return lval_err("S-expression does not start with symbol!");
	}

	/* Call builtin with operator */
	lval* result = builtin(v, f->sym);
	lval_del(f);
	return result;
}


int main(int argc, char** argv) {

	/* create parsers */
	mpc_parser_t* Number = mpc_new("number");
	mpc_parser_t* Symbol = mpc_new("symbol");
	mpc_parser_t* Sexpr = mpc_new("sexpr");
	mpc_parser_t* Qexpr = mpc_new("qexpr");
	mpc_parser_t* Expr = mpc_new("expr");
	mpc_parser_t* Lispy = mpc_new("lispy");

	/* define parsers with the following language */
	mpca_lang(MPCA_LANG_DEFAULT, 
			" 			                              		\
			number   : /-?[0-9]+/ ; 			     		\
			symbol : '+' | '-' | '*' | '/' | '^'                   	        \
				| \"list\" | \"head\" | \"tail\" | \"join\" | \"eval\"; \
			sexpr : '(' <expr>* ')' ; 			     		\
			qexpr : '{' <expr>* '}' ;  			     		\
			expr     : <number> | <symbol> | <sexpr> | <qexpr>;  		\
			lispy    : /^/ <expr>* /$/ ; 	     		     		\
			",
			Number, Symbol, Sexpr, Qexpr, Expr, Lispy);

	/* Print Version and Exit Information */
	puts("Lispy Version 0.0.0.0.1");
	puts("Press Ctrl+c to Exit\n");

	/* In a never ending loop */
	while (1) {

		/* Output our prompt and get input */
		char* input = readline("lispy> ");

		/* Add input to history */
		add_history(input);

		/* Attempt to parse the user input */
		mpc_result_t r;
		if (mpc_parse("<stdin>", input, Lispy, &r)) {
			/* On Success print the ast */
			lval* x = lval_eval(lval_read(r.output));
			lval_println(x);
			lval_del(x);
			mpc_ast_delete(r.output);
		} else {
			/* Otherwise print the error */
			mpc_err_print(r.error);
			mpc_err_delete(r.error);
		}

		/* Free retrieved input */
		free(input);

	}

	/* Undefine and Delete parsers */
	mpc_cleanup(5, Number, Symbol, Sexpr, Qexpr, Expr, Lispy);

	return 0;
}
