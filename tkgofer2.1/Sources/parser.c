
# line 19 "parser.y"
#ifndef lint
#define lint
#endif
#define defTycon(n,l,lhs,rhs,w)	 tyconDefn(intOf(l),lhs,rhs,w); sp-=n
#define sigdecl(l,vs,t)		 ap(SIGDECL,triple(l,vs,t))
#define grded(gs)		 ap(GUARDED,gs)
#define letrec(bs,e)		 (nonNull(bs) ? ap(LETREC,pair(bs,e)) : e)
#define yyerror(s)		 /* errors handled elsewhere */
#define YYSTYPE			 Cell

static Cell   local gcShadow     Args((Int,Cell));
static Void   local syntaxError  Args((String));
static String local unexpected   Args((Void));
static Cell   local checkPrec    Args((Cell));
static Void   local fixDefn      Args((Syntax,Cell,Cell,List));
static Void   local setSyntax    Args((Int,Syntax,Cell));
static Cell   local buildTuple   Args((List));
static Cell   local checkClass   Args((Cell));
static List   local checkContext Args((List));
static Pair   local checkDo	 Args((List));
static Cell   local checkTyLhs	 Args((Cell));
static Cell   local tidyInfix    Args((Cell));

/* For the purposes of reasonably portable garbage collection, it is
 * necessary to simulate the YACC stack on the Gofer stack to keep
 * track of all intermediate constructs.  The lexical analyser
 * pushes a token onto the stack for each token that is found, with
 * these elements being removed as reduce actions are performed,
 * taking account of look-ahead tokens as described by gcShadow()
 * below.
 *
 * Of the non-terminals used below, only start, topDecl & begin do not leave
 * any values on the Gofer stack.  The same is true for the terminals
 * EVALEX and SCRIPT.  At the end of a successful parse, there should only
 * be one element left on the stack, containing the result of the parse.
 */

#define gc0(e)			 gcShadow(0,e)
#define gc1(e)			 gcShadow(1,e)
#define gc2(e)			 gcShadow(2,e)
#define gc3(e)			 gcShadow(3,e)
#define gc4(e)			 gcShadow(4,e)
#define gc5(e)			 gcShadow(5,e)
#define gc6(e)			 gcShadow(6,e)
#define gc7(e)			 gcShadow(7,e)

# define EVALEX 257
# define SCRIPT 258
# define COCO 259
# define INFIXL 260
# define INFIXR 261
# define INFIX 262
# define FUNARROW 263
# define UPTO 264
# define CASEXP 265
# define OF 266
# define IF 267
# define THEN 268
# define ELSE 269
# define WHERE 270
# define TYPE 271
# define DATA 272
# define FROM 273
# define LET 274
# define IN 275
# define VAROP 276
# define VARID 277
# define NUMLIT 278
# define CHARLIT 279
# define STRINGLIT 280
# define REPEAT 281
# define CONOP 282
# define CONID 283
# define TCLASS 284
# define IMPLIES 285
# define TINSTANCE 286
# define DO 287
# define TRUNST 288
# define PRIMITIVE 289
# define DEFAULT 290
# define DERIVING 291
# define HIDING 292
# define IMPORT 293
# define INTERFACE 294
# define MODULE 295
# define RENAMING 296
# define TO 297

#ifdef __STDC__
#include <stdlib.h>
#include <string.h>
#else
#include <malloc.h>
#include <memory.h>
#endif

#include <values.h>

#ifdef __cplusplus

#ifndef yyerror
	void yyerror(const char *);
#endif

#ifndef yylex
#ifdef __EXTERN_C__
	extern "C" { int yylex(void); }
#else
	int yylex(void);
#endif
#endif
	int yyparse(void);

#endif
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYSTYPE
#define YYSTYPE int
#endif
YYSTYPE yylval;
YYSTYPE yyval;
typedef int yytabelem;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
#if YYMAXDEPTH > 0
int yy_yys[YYMAXDEPTH], *yys = yy_yys;
YYSTYPE yy_yyv[YYMAXDEPTH], *yyv = yy_yyv;
#else	/* user does initial allocation */
int *yys;
YYSTYPE *yyv;
#endif
static int yymaxdepth = YYMAXDEPTH;
# define YYERRCODE 256

# line 488 "parser.y"


static Cell local gcShadow(n,e)		/* keep parsed fragments on stack  */
Int  n;
Cell e; {
    /* If a look ahead token is held then the required stack transformation
     * is:
     *   pushed: n               1     0          1     0
     *           x1  |  ...  |  xn  |  la   ===>  e  |  la
     *                                top()            top()
     *
     * Othwerwise, the transformation is:
     *   pushed: n-1             0        0
     *           x1  |  ...  |  xn  ===>  e
     *                         top()     top()
     */
    if (yychar>=0) {
	pushed(n-1) = top();
        pushed(n)   = e;
    }
    else
	pushed(n-1) = e;
    sp -= (n-1);
    return e;
}

static Void local syntaxError(s)       /* report on syntax error           */
String s; {
    ERROR(row) "Syntax error in %s (unexpected %s)", s, unexpected()
    EEND;
}

static String local unexpected() {	/* find name for unexpected token  */
    static char buffer[100];
    static char *fmt = "%s \"%s\"";
    static char *kwd = "keyword";
    static char *hkw = "(Haskell) keyword";

    switch (yychar) {
	case 0	       : return "end of input";

#define keyword(kw) sprintf(buffer,fmt,kwd,kw); return buffer;
	case INFIXL    : keyword("infixl");
	case INFIXR    : keyword("infixr");
	case INFIX     : keyword("infix");
	case TINSTANCE : keyword("instance");
	case TCLASS    : keyword("class");
	case PRIMITIVE : keyword("primitive");
	case CASEXP    : keyword("case");
	case OF        : keyword("of");
	case IF        : keyword("if");
	case DO	       : keyword("do");
	case TRUNST    : keyword("runST");
	case THEN      : keyword("then");
	case ELSE      : keyword("else");
	case WHERE     : keyword("where");
	case TYPE      : keyword("type");
	case DATA      : keyword("data");
	case LET       : keyword("let");
	case IN        : keyword("in");
#undef keyword

#define hasword(kw) sprintf(buffer,fmt,hkw,kw); return buffer;
	case DEFAULT   : hasword("default");
	case DERIVING  : hasword("deriving");
	case HIDING    : hasword("hiding");
	case IMPORT    : hasword("import");
	case INTERFACE : hasword("interface");
	case MODULE    : hasword("module");
	case RENAMING  : hasword("renaming");
	case TO	       : hasword("to");
#undef hasword

	case FUNARROW  : return "`->'";
	case '='       : return "`='";
	case COCO      : return "`::'";
	case '-'       : return "`-'";
	case ','       : return "comma";
	case '@'       : return "`@'";
	case '('       : return "`('";
	case ')'       : return "`)'";
	case '|'       : return "`|'";
	case ';'       : return "`;'";
	case UPTO      : return "`..'";
	case '['       : return "`['";
	case ']'       : return "`]'";
	case FROM      : return "`<-'";
	case '\\'      : return "backslash (lambda)";
	case '~'       : return "tilde";
	case '`'       : return "backquote";
	case VAROP     :
	case VARID     :
	case CONOP     :
	case CONID     : sprintf(buffer,"symbol \"%s\"",
				 textToStr(textOf(yylval)));
			 return buffer;
	case NUMLIT    : return "numeric literal";
	case CHARLIT   : return "character literal";
	case STRINGLIT : return "string literal";
	case IMPLIES   : return "`=>";
	default	       : return "token";
    }
}

static Cell local checkPrec(p)         /* Check for valid precedence value */
Cell p; {
    if (!isInt(p) || intOf(p)<MIN_PREC || intOf(p)>MAX_PREC) {
        ERROR(row) "Precedence value must be an integer in the range [%d..%d]",
                   MIN_PREC, MAX_PREC
        EEND;
    }
    return p;
}

static Void local fixDefn(a,line,p,ops)/* Declare syntax of operators      */
Syntax a;
Cell   line;
Cell   p;
List   ops; {
    Int l = intOf(line);
    a     = mkSyntax(a,intOf(p));
    map2Proc(setSyntax,l,a,ops);
}

static Void local setSyntax(line,sy,op)/* set syntax of individ. operator  */
Int    line;
Syntax sy;
Cell   op; {
    addSyntax(line,textOf(op),sy);
    opDefns = cons(op,opDefns);
}

static Cell local buildTuple(tup)      /* build tuple (x1,...,xn) from list*/
List tup; {                            /* [xn,...,x1]                      */
    Int  n = 0;
    Cell t = tup;
    Cell x;

    do {                               /*     .                    .       */
        x      = fst(t);               /*    / \                  / \      */
        fst(t) = snd(t);               /*   xn  .                .   xn    */
        snd(t) = x;                    /*        .    ===>      .          */
        x      = t;                    /*         .            .           */
        t      = fun(x);               /*          .          .            */
        n++;                           /*         / \        / \           */
    } while (nonNull(t));              /*        x1  NIL   (n)  x1         */
    fst(x) = mkTuple(n);
    return tup;
}

/* The yacc parser presented above is not sufficiently powerful to
 * determine whether a tuple at the front of a sigType is part of a
 * context:    e.g. (Eq a, Num a) => a -> a -> a
 * or a type:  e.g.  (Tree a, Tree a) -> Tree a
 *
 * Rather than complicate the grammar, both are parsed as tuples of types,
 * using the following checks afterwards to ensure that the correct syntax
 * is used in the case of a tupled context.
 */

static List local checkContext(con)	/* validate type class context	   */
Type con; {
    if (con==UNIT)			/* allows empty context ()	   */
	return NIL;
    else if (whatIs(getHead(con))==TUPLE) {
	List qs = NIL;

	while (isAp(con)) {		/* undo work of buildTuple  :-(    */
	    Cell temp = fun(con);
	    fun(con)  = arg(con);
	    arg(con)  = qs;
	    qs	      = con;
	    con       = temp;
	    checkClass(hd(qs));
	}
	return qs;
    }
    else				/* single context expression	   */
	return singleton(checkClass(con));
}

static Cell local checkClass(c)		/* check that type expr is a class */
Cell c; {				/* constrnt of the form C t1 .. tn */
    Cell cn = getHead(c);

    if (!isCon(cn))
	syntaxError("class expression");
    else if (argCount<1) {
	ERROR(row) "Class \"%s\" must have at least one argument",
		   textToStr(textOf(cn))
	EEND;
    }
    return c;
}

static Pair local checkDo(dqs)		/* convert reversed list of dquals */
List dqs; {				/* to a (expr,quals) pair	   */
#if DO_COMPS
    if (isNull(dqs) || whatIs(hd(dqs))!=DOQUAL) {
	ERROR(row) "Last generator in do {...} must be an expression"
	EEND;
    }
    fst(dqs) = snd(fst(dqs));		/* put expression in fst of pair   */
    snd(dqs) = rev(snd(dqs));		/* & reversed list of quals in snd */
#endif
    return dqs;
}

static Cell local checkTyLhs(c)		/* check that lhs is of the form   */
Cell c; {				/* T a1 ... a			   */
    Cell tlhs = c;
    while (isAp(tlhs) && whatIs(arg(tlhs))==VARIDCELL)
	tlhs = fun(tlhs);
    if (whatIs(tlhs)!=CONIDCELL) {
	ERROR(row) "Illegal left hand side in datatype definition"
	EEND;
    }
    return c;
}

/* expressions involving a sequence of two or more infix operator symbols
 * are parsed as elements of type:
 *    InfixExpr ::= [Expr]
 *		 |  ap(ap(Operator,InfixExpr),Expr)
 *
 * thus x0 +1 x1 ... +n xn is parsed as: +n (....(+1 [x0] x1)....) xn
 *
 * Once the expression has been completely parsed, this parsed form is
 * `tidied' according to the precedences and associativities declared for
 * each operator symbol.
 *
 * The tidy process uses a `stack' of type:
 *    TidyStack ::= ap(ap(Operator,TidyStack),Expr)
 *		 |  NIL
 * when the ith layer of an InfixExpr has been transferred to the stack, the
 * stack is of the form: +i (....(+n NIL xn)....) xi
 *
 * The tidy function is based on a simple shift-reduce parser:
 *
 *  tidy                :: InfixExpr -> TidyStack -> Expr
 *  tidy [m]   ss        = foldl (\x f-> f x) m ss
 *  tidy (m*n) []        = tidy m [(*n)]
 *  tidy (m*n) ((+o):ss)
 *	       | amb     = error "Ambiguous"
 *	       | shift   = tidy m ((*n):(+o):ss)
 *	       | reduce  = tidy (m*(n+o)) ss
 *			   where sye     = syntaxOf (*)
 *				 (ae,pe) = sye
 *				 sys     = syntaxOf (+)
 *				 (as,ps) = sys
 *				 amb     = pe==ps && (ae/=as || ae==NON_ASS)
 *				 shift   = pe>ps || (ps==pe && ae==LEFT_ASS)
 *				 reduce  = otherwise
 *
 * N.B. the conditions amb, shift, reduce are NOT mutually exclusive and
 * must be tested in that order.
 *
 * As a concession to efficiency, we lower the number of calls to syntaxOf
 * by keeping track of the values of sye, sys throughout the process.  The
 * value APPLIC is used to indicate that the syntax value is unknown.
 */

static Cell local tidyInfix(e)         /* convert InfixExpr to Expr        */
Cell e; {                              /* :: InfixExpr                     */
    Cell   s   = NIL;                  /* :: TidyStack                     */
    Syntax sye = APPLIC;               /* Syntax of op in e (init unknown) */
    Syntax sys = APPLIC;               /* Syntax of op in s (init unknown) */
    Cell   temp;

    while (nonNull(tl(e))) {
        if (isNull(s)) {
            s           = e;
            e           = arg(fun(s));
            arg(fun(s)) = NIL;
            sys         = sye;
            sye         = APPLIC;
        }
        else {
            if (sye==APPLIC) {         /* calculate sye (if unknown)       */
                sye = syntaxOf(textOf(fun(fun(e))));
                if (sye==APPLIC) sye=DEF_OPSYNTAX;
            }
            if (sys==APPLIC) {         /* calculate sys (if unknown)       */
                sys = syntaxOf(textOf(fun(fun(s))));
                if (sys==APPLIC) sys=DEF_OPSYNTAX;
            }

            if (precOf(sye)==precOf(sys) &&                      /* amb    */
                   (assocOf(sye)!=assocOf(sys) || assocOf(sye)==NON_ASS)) {
                ERROR(row) "Ambiguous use of operator \"%s\" with \"%s\"",
                           textToStr(textOf(fun(fun(e)))),
                           textToStr(textOf(fun(fun(s))))
                EEND;
            }
            else if (precOf(sye)>precOf(sys) ||                  /* shift  */
                       (precOf(sye)==precOf(sys) && assocOf(sye)==LEFT_ASS)) {
                temp        = arg(fun(e));
                arg(fun(e)) = s;
                s           = e;
                e           = temp;
                sys         = sye;
                sye         = APPLIC;
            }
            else {                                               /* reduce */
                temp        = arg(fun(s));
                arg(fun(s)) = arg(e);
                arg(e)      = s;
                s           = temp;
                sys         = APPLIC;
                /* sye unchanged */
            }
        }
    }

    e = hd(e);
    while (nonNull(s)) {
        temp        = arg(fun(s));
        arg(fun(s)) = e;
        e           = s;
        s           = temp;
    }

    return e;
}

/*-------------------------------------------------------------------------*/
yytabelem yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 28,
	93, 198,
	-2, 0,
-1, 88,
	259, 143,
	44, 143,
	-2, 169,
-1, 97,
	285, 75,
	-2, 74,
-1, 134,
	93, 204,
	-2, 0,
-1, 158,
	285, 75,
	-2, 118,
-1, 246,
	264, 19,
	-2, 41,
-1, 269,
	93, 203,
	-2, 0,
-1, 285,
	96, 78,
	282, 78,
	-2, 65,
	};
# define YYNPROD 223
# define YYLAST 979
yytabelem yyact[]={

    19,     6,    22,    95,   335,   364,    97,   327,   117,   358,
   137,     5,   282,   283,    86,   313,   112,   385,   243,   135,
    38,   264,    53,    54,   242,   384,   275,    35,    37,   328,
   214,    72,    64,   220,    88,    87,    44,    70,   230,   218,
   173,   404,    41,    48,   395,   111,   141,   144,    43,   106,
    74,    91,   390,    88,    87,   107,   107,   331,   380,   226,
   225,   148,   261,   103,   326,   195,    37,   368,   103,   302,
    93,   349,    68,    92,   143,    93,    39,   272,    92,   240,
   232,   205,   229,   154,   262,   145,   113,    46,   278,   114,
   158,   158,   160,   299,   269,    88,    87,   155,   381,   387,
   128,     4,     2,     3,   155,    40,   219,   349,   168,   314,
   178,   183,    73,   238,   104,   296,   103,   314,   188,   104,
   332,   292,   191,   103,   226,   290,   193,    98,   197,   273,
   196,   190,   155,   103,   198,   215,   199,    88,    87,   185,
   172,   203,   206,   208,   192,   155,   187,   211,   132,    94,
   186,   258,    56,   210,   131,   152,   224,    52,    58,   295,
    96,   156,   217,   234,   162,   233,   403,   104,    15,   184,
   147,   386,   245,   244,   104,   236,   169,   162,   239,    49,
   248,   249,   142,    23,   104,   235,   382,   402,    10,   383,
    88,    87,   247,   293,   265,   324,   329,    88,    87,   287,
   211,   270,   166,   127,   267,   377,   260,   116,   378,   123,
   379,    90,   103,   176,   361,   245,   182,   324,   376,   136,
   297,   103,   281,   298,   284,   154,   323,   288,   133,    49,
   122,   179,   121,   279,    28,    11,   250,   291,    21,   146,
   202,   312,   268,    47,   157,   157,   159,   294,   311,   209,
   140,   149,   150,   201,   115,   256,   221,   222,   257,   301,
   249,   300,   321,   104,   303,   169,   276,    45,   138,    20,
   304,   305,   104,    47,    88,    87,   237,   245,   316,   206,
   318,   319,   213,   306,    99,   138,   253,    30,   315,   271,
   227,    88,    87,    88,    87,   279,   380,    88,    87,   245,
   101,   350,   340,   347,   341,   101,   102,   336,    31,   265,
   367,   102,    23,    74,    30,   348,   355,    10,   211,   320,
   354,   333,   359,   343,   360,   245,   353,   365,   342,   351,
   352,   344,    99,   330,    31,   284,   216,   369,   338,   284,
   356,    31,   366,   362,    31,   370,   174,   280,   374,    99,
    30,   216,   317,   101,   371,   227,    23,   121,   373,   102,
   101,   153,   375,    28,    11,   322,   102,    21,   134,    31,
   101,   223,    93,    88,    87,   246,   102,   392,   391,   161,
   289,   394,    31,   211,   359,   398,   360,   365,   399,   336,
   396,   401,   400,   397,   189,    23,    63,   165,    20,   139,
    60,   286,   266,    79,    80,    81,   372,    28,    14,   307,
    13,    21,   194,   171,    77,    78,   139,    12,   204,    36,
    31,    24,    25,    26,    27,   310,    30,    83,    99,    84,
    16,    17,    82,    85,    23,   177,    76,   285,   254,    10,
   251,   255,    20,   252,    65,   126,    28,    11,   127,   101,
    21,    46,    42,   124,    89,   102,   125,   309,   101,     8,
   138,   308,    23,    69,   102,   263,    50,    10,     9,    29,
   167,   164,   138,    71,   100,   138,    67,   163,   138,   334,
   231,    20,   228,    66,   151,    28,    11,   181,   180,    21,
    23,   389,   388,   363,   325,    10,   346,   345,   357,   277,
   241,   105,   274,   212,   170,    34,    33,    32,   108,     1,
     0,     0,     0,    28,    11,     0,    23,    21,     0,   138,
    20,    10,     0,     0,     0,     0,     0,     0,    75,     0,
     0,     0,    79,    80,    81,    23,     0,    14,     0,    13,
    10,    28,    11,    77,    78,    21,    12,   138,    20,    31,
    24,    25,    26,    27,     0,    30,    83,     0,    84,    16,
    17,    82,    85,     0,     0,    76,     0,    28,    11,     0,
     0,    21,    23,   175,     0,     0,    20,    10,     0,     0,
     0,     0,     0,     0,     0,     0,    28,    11,   105,     0,
    21,   139,     0,    31,    24,    25,    26,    27,    18,    30,
    23,     0,    20,   139,    17,    10,   139,     0,     0,   139,
    51,     7,     0,     0,    55,     0,    57,     0,     0,    59,
    14,    20,    13,    28,    11,     0,     0,    21,     0,    12,
     0,    61,    31,    24,    25,    26,    27,    62,    30,    23,
   200,     0,    16,    17,    10,   259,     0,     0,    55,   110,
   393,    28,    11,    23,   175,    21,     0,   120,    20,    14,
     0,    13,     0,     0,     0,     0,   129,   130,    12,     0,
     0,    31,    24,    25,    26,    27,     0,    30,   139,     0,
     0,    16,    17,     0,     0,    23,    20,    14,     0,    13,
    28,    11,     0,    23,    21,     0,    12,     0,    10,    31,
    24,    25,    26,    27,    28,    30,     7,     0,    21,    16,
    17,     0,     0,     0,     0,    14,     0,    13,     0,     0,
     0,     0,     0,     0,    12,    20,     0,    31,    24,    25,
    26,    27,     7,    30,     0,    23,    28,    16,    17,    20,
    21,    14,     0,   339,    28,    11,     0,     0,    21,     0,
    12,   337,     0,    31,    24,    25,    26,    27,     0,    30,
    14,     0,    13,    16,    17,     0,     0,     0,     0,    12,
     0,    20,    31,    24,    25,    26,    27,     0,    30,    20,
     0,     0,    16,    17,     0,     0,    28,     0,     7,     0,
    21,     0,     0,     0,     0,     0,     0,    14,     0,    13,
     0,     0,     0,     0,     0,     0,   207,     0,     0,    31,
    24,    25,    26,    27,     0,    30,     7,     0,     0,    16,
    17,    20,     0,     0,     0,    14,     0,   119,     0,     0,
     0,     0,     0,     0,   118,     0,     0,    31,    24,    25,
    26,    27,     0,    30,     0,     0,     0,    16,    17,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,    14,     0,    13,     0,     0,     0,
     0,     0,     0,    12,     0,     0,    31,    24,    25,    26,
    27,     0,    30,     0,     0,     0,    16,    17,     0,     0,
    31,    24,    25,    26,    27,     0,    30,     0,     0,     0,
     0,    17,     0,     0,     0,     0,     0,     0,   109,     0,
     0,     0,     0,     0,     0,     0,     0,     0,    14,     0,
    13,     0,    31,    24,    25,    26,    27,    12,    30,     0,
    31,    24,    25,    26,    27,     0,    30,     0,     0,     0,
    16,    17,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,    31,    24,    25,    26,    27,     0,    30 };
yytabelem yypact[]={

  -155,-10000000,   450,  -229,-10000000,  -194,  -154,-10000000,    -9,    -9,
   613,   695,    34,   450,   450,   695,    29,   695,-10000000,    94,
   695,-10000000,-10000000,   355,-10000000,-10000000,-10000000,-10000000,   450,-10000000,
-10000000,-10000000,-10000000,   272,  -267,-10000000,-10000000,  -205,-10000000,    26,
    93,   653,-10000000,-10000000,-10000000,-10000000,  -228,-10000000,   653,   695,
   645,-10000000,   653,  -182,  -177,-10000000,   560,-10000000,   695,-10000000,
   316,   189,   168,-10000000,   412,   404,    -9,   695,   695,    61,
   104,   159,   160,-10000000,-10000000,-10000000,  -210,  -209,    93,  -217,
  -217,  -217,   105,    93,    93,    93,   120,   141,    94,-10000000,
   373,-10000000,-10000000,-10000000,   653,-10000000,  -245,-10000000,    83,-10000000,
-10000000,-10000000,-10000000,   172,    76,    -9,    54,    50,-10000000,   450,
-10000000,   335,-10000000,   450,    21,   353,-10000000,  -208,     7,   450,
-10000000,-10000000,-10000000,-10000000,-10000000,   450,-10000000,   450,   599,   212,
   199,-10000000,   450,   532,   450,-10000000,   143,-10000000,-10000000,-10000000,
   -10,-10000000,    74,-10000000,-10000000,   101,  -246,    -9,-10000000,    -9,
    -9,   112,-10000000,-10000000,  -220,    79,  -188,  -247,-10000000,  -190,
-10000000,    93,    57,-10000000,  -194,-10000000,   450,    52,-10000000,   450,
  -191,    92,   335,    93,    93,-10000000,-10000000,   195,   399,    23,
   397,   214,-10000000,    58,-10000000,   653,-10000000,-10000000,-10000000,   422,
  -213,  -185,   653,-10000000,   560,   450,   653,  -182,-10000000,-10000000,
-10000000,-10000000,-10000000,  -170,   157,-10000000,    16,     6,-10000000,-10000000,
-10000000,-10000000,  -270,   226,    64,    93,-10000000,   181,  -209,   155,
-10000000,   155,   155,    93,    57,-10000000,   191,   189,-10000000,     2,
    93,-10000000,    -2,-10000000,-10000000,-10000000,   149,-10000000,   450,    98,
    -8,   179,-10000000,-10000000,  -171,-10000000,   221,-10000000,-10000000,-10000000,
-10000000,-10000000,    93,    28,-10000000,-10000000,-10000000,    93,-10000000,-10000000,
-10000000,   450,   450,   350,-10000000,   -15,-10000000,-10000000,   335,   450,
   532,   450,   450,   653,-10000000,   222,    64,   185,   151,-10000000,
   221,  -211,   -95,-10000000,   -39,-10000000,    59,    -9,-10000000,-10000000,
   495,-10000000,   653,   476,   149,   450,   272,-10000000,    92,-10000000,
    67,-10000000,-10000000,-10000000,-10000000,-10000000,-10000000,   422,-10000000,  -194,
    -7,   450,-10000000,-10000000,   653,  -213,-10000000,-10000000,-10000000,-10000000,
   335,    31,   173,-10000000,    64,-10000000,    57,-10000000,   181,    27,
    93,  -227,   181,-10000000,   347,-10000000,-10000000,-10000000,   335,   450,
-10000000,-10000000,   160,-10000000,   177,   164,   169,-10000000,   133,    14,
-10000000,-10000000,-10000000,-10000000,-10000000,  -165,  -213,   145,-10000000,  -272,
  -280,-10000000,-10000000,   127,-10000000,  -160,-10000000,-10000000,  -231,-10000000,
   -95,-10000000,   394,-10000000,  -182,-10000000,-10000000,-10000000,     4,-10000000,
   168,   450,-10000000,    31,    57,     4,    57,    93,   146,   122,
-10000000,-10000000,-10000000,-10000000,-10000000,  -224,-10000000,-10000000,-10000000,-10000000,
-10000000,-10000000,-10000000,  -242,-10000000 };
yytabelem yypgo[]={

     0,   509,     8,    20,   507,   506,    31,    19,   505,   112,
    16,   419,   173,   504,   503,   502,   500,    24,    18,    88,
   499,   498,     9,     0,     2,   497,   496,    14,   182,     6,
   494,    12,     7,   160,   493,     5,     3,    13,    48,   492,
   491,   127,   474,   488,   487,   170,   106,    33,   452,   484,
   155,   161,   482,   480,   479,    45,     4,     1,   477,   471,
   470,   108,   469,   459,   468,   168,   466,   465,   598,   444,
   463,    21,   461,   457,   425,    15,   418,    81,   254,    10,
   207 };
yytabelem yyr1[]={

     0,     1,     1,     1,     1,     4,     4,     5,     6,     6,
     6,     6,     6,     8,     8,    11,    11,     9,     9,    12,
    12,    13,    13,    16,    16,    17,    17,    14,    14,    14,
    20,    20,    19,    19,    15,    15,    21,    21,    22,    22,
    18,    18,    18,    18,    18,    25,    25,    26,    26,     9,
     9,     9,    28,    28,    28,    30,    30,    34,    34,    35,
    35,    31,    31,    37,    37,    37,    32,    32,    32,    39,
    39,    40,    40,    36,    36,    33,    29,    29,    29,    41,
    41,    42,    42,    42,    42,    42,    42,    42,    42,    42,
    42,    43,    43,    44,    44,     9,     9,     9,    45,    45,
    46,    46,    47,    47,    47,    48,    48,    38,    38,     9,
    49,    49,    49,    50,     9,     9,     9,    51,    51,    52,
    52,    53,    53,    54,    54,    56,    56,    10,    10,    55,
    55,    58,    58,    58,    59,    59,     3,    60,    60,    61,
    61,    61,    27,    27,    23,    23,    62,    62,    24,    24,
     2,     2,     2,    57,    57,    57,    64,    64,    63,    63,
    63,    63,    63,    63,    66,    66,    65,    65,    65,    68,
    68,    68,    68,    68,    68,    68,    68,    68,    68,    68,
    68,    68,    68,    68,    68,    69,    69,    67,    67,    71,
    72,    72,    73,    73,    73,    74,    74,    75,    70,    70,
    70,    70,    70,    70,    70,    70,    76,    76,    77,    77,
    77,    77,    63,    80,    80,    80,    80,    78,    78,     7,
     7,    79,    79 };
yytabelem yyr2[]={

     0,     5,     7,     5,     3,     7,     3,     3,     7,     7,
     3,     3,     3,     5,     3,    15,     5,     9,     5,     3,
     3,     1,     7,     7,     3,     3,     5,     1,     9,     7,
     1,     3,     7,     3,     1,     9,     7,     3,     7,     7,
     3,     3,     9,     9,     9,     7,     3,     1,     3,    11,
    11,    15,     5,     3,     3,     5,     1,     7,     3,     7,
     3,     7,     3,     7,     3,     3,     1,     5,     9,     1,
     3,     7,     3,     7,     3,     3,     3,     7,     3,     5,
     3,     3,     3,     5,     7,     7,     9,     7,     7,     7,
     5,     5,     3,     7,     7,     7,     7,     7,     3,     1,
     7,     3,     3,     3,     3,     3,     7,     3,     7,     9,
     7,     3,     3,     5,     7,     7,     5,     7,     3,     9,
     1,     9,     1,     7,     3,     3,     3,     7,     5,     7,
     3,     5,     3,     3,     5,     3,     9,     5,     3,     9,
    11,     9,     7,     3,     3,     7,     3,     7,     3,     7,
     7,     3,     3,     3,     7,     3,     7,    11,     5,     9,
    13,    13,    13,     3,     5,     3,     5,     5,     3,     3,
     7,     5,     3,     3,     5,     3,     3,     3,     3,     7,
     7,     7,     9,     9,     9,     7,     7,     7,     3,     5,
     5,     3,     3,     5,     3,     5,     3,     9,     1,     3,
     3,     7,     7,     9,     5,    11,     7,     3,     7,     7,
     3,     9,     9,     7,     9,     5,     3,     7,     3,     5,
     3,     3,     3 };
yytabelem yychk[]={

-10000000,    -1,   257,   258,   256,    -2,   -57,   256,   -63,   -64,
    45,    92,   274,   267,   265,   -65,   287,   288,   -68,   -23,
   126,    95,   -24,    40,   278,   279,   280,   281,    91,   -62,
   283,   277,    -4,    -5,    -8,   256,   -11,   295,    -3,   270,
   259,   -47,   -48,   -38,    45,   276,    96,   282,   -47,   -65,
   -66,   -68,   123,    -2,    -2,   -68,   123,   -68,    64,   -68,
    45,   276,   282,    41,    -2,   -69,   -63,   -48,   -38,   -70,
    -2,   -69,    -6,    -9,   -10,   256,   293,   271,   272,   260,
   261,   262,   289,   284,   286,   290,   -27,   -57,   -23,   -11,
   -12,   256,   283,   280,   123,   -36,   -33,   -29,   -41,   256,
   -42,   277,   283,    40,    91,   -63,   277,   283,   -63,   263,
   -68,   -55,   -10,   268,   266,   -78,   -80,    -2,   274,   267,
   -68,    41,    41,    41,    41,    44,    41,    44,   -47,   -68,
   -68,    93,    44,   124,   264,    -7,    59,   -79,   125,   256,
   -12,   256,   -28,   283,   256,   -29,   -33,   -45,   278,   -45,
   -45,   -49,   -50,   256,   -23,    40,   -51,   -33,   -29,   -51,
   -29,   259,    44,   -58,   -59,   256,    61,   -60,   -61,   124,
   -13,    40,   -55,   285,   263,   -42,    41,   263,   -29,   -41,
   -43,   -44,    44,   -29,    93,   -47,    96,    96,    -2,    59,
    -7,    -2,   123,   -79,    59,   273,   123,    -2,    -2,    -2,
    41,    41,    41,    -2,   -76,   -77,    -2,   274,    -2,    -9,
   -10,   -79,   -14,   292,    40,    61,   277,    61,   285,   -46,
   -47,   -46,   -46,   259,    44,   280,    45,   276,   -52,   270,
   285,   -53,   270,   -36,   -23,    -3,    -2,   -61,    61,    -2,
   270,   -16,   -17,   -18,   -12,   -23,   283,    -7,   -29,   -29,
    41,    41,    44,   263,    41,    44,    41,    44,    93,   -63,
   -10,   275,   269,   -67,   -71,   -57,   -80,    -2,   -55,   264,
    44,   273,    61,   123,   -15,   296,    40,   -20,   -19,   -18,
   283,   -29,   -31,   -37,   -29,   256,   -28,    44,   -36,   -50,
   123,   -29,   123,    44,    -2,    61,   123,    41,    44,   264,
    40,   -29,    41,   -29,    -2,    -2,    -7,    59,   -72,   -73,
   -74,   263,   256,   -75,   124,    -7,    -2,   -77,    -2,    -2,
   -55,    40,   -19,    41,    44,   -30,   275,   -32,   124,   291,
   -38,    96,    61,   -47,   -54,   -56,   -10,   256,   -55,   267,
    -2,    -2,    -6,   -17,   264,   -25,   -26,   -24,   -27,    40,
   -23,   -71,    -3,   -75,    -2,   -57,    -7,   -21,   -22,   -23,
   -24,    41,   -18,   -34,   -35,   -23,   -37,   283,    40,   -29,
   -31,    -7,    59,    -7,    -2,    -7,    41,    41,    44,    41,
   282,   263,    41,    44,   297,   297,    44,   259,   -39,   -40,
   283,   -32,   -56,   256,   -24,    40,    -2,   -22,   -23,   -24,
   -35,   -36,    41,    44,   283 };
yytabelem yydef[]={

     0,    -2,     0,     0,     4,     1,   151,   152,   153,   155,
     0,     0,     0,     0,     0,   163,     0,     0,   168,   169,
     0,   172,   173,     0,   175,   176,   177,   178,    -2,   144,
   148,   146,     3,     0,     6,     7,    14,     0,     2,     0,
     0,     0,   102,   103,   104,   105,     0,   107,     0,   158,
     0,   165,     0,     0,     0,   166,     0,   167,     0,   171,
     0,   105,   107,   174,     0,     0,   153,     0,     0,     0,
   199,   200,     0,    10,    11,    12,     0,     0,     0,    99,
    99,    99,     0,     0,     0,     0,     0,     0,    -2,    13,
    21,    16,    19,    20,     0,   150,     0,    -2,    76,    78,
    80,    81,    82,     0,     0,   154,     0,     0,   156,     0,
   164,     0,   130,     0,     0,     0,   218,   216,     0,     0,
   170,   145,   147,   149,   179,     0,   180,     0,     0,     0,
     0,   181,     0,     0,    -2,     5,     0,   220,   221,   222,
    27,    18,     0,    53,    54,    75,     0,     0,    98,     0,
     0,     0,   111,   112,     0,     0,   120,     0,    -2,   122,
   116,     0,     0,   128,   132,   133,     0,   135,   138,     0,
     0,     0,     0,     0,     0,    79,    83,     0,     0,    76,
     0,     0,    92,     0,    90,     0,   106,   108,   159,     0,
     0,     0,     0,   212,     0,     0,     0,   215,   186,   185,
   182,   183,   184,   186,   201,   207,   210,     0,   202,     8,
     9,   219,    34,     0,    30,     0,    52,     0,     0,    95,
   101,    96,    97,     0,     0,   113,     0,     0,   114,     0,
     0,   115,     0,   127,   142,   131,   134,   137,     0,     0,
     0,     0,    24,    25,     0,    40,    -2,   136,    73,    77,
    84,    85,     0,     0,    87,    91,    88,     0,    89,   157,
   129,     0,     0,     0,   188,     0,   217,   213,     0,    -2,
     0,     0,     0,     0,    17,     0,     0,     0,    31,    33,
    41,    56,    66,    62,    64,    -2,     0,     0,   109,   110,
     0,   117,     0,     0,     0,     0,     0,    22,     0,    26,
    47,    94,    86,    93,   160,   161,   162,     0,   189,   191,
   192,     0,   194,   196,     0,   214,   205,   206,   208,   209,
     0,     0,     0,    29,     0,    49,     0,    50,     0,     0,
     0,     0,     0,   100,     0,   124,   125,   126,     0,     0,
   141,   139,     0,    23,     0,     0,     0,    46,    48,     0,
   143,   187,   190,   195,   193,     0,   211,     0,    37,     0,
     0,    28,    32,    55,    58,    60,    61,    67,    69,    63,
    66,   119,     0,   121,   140,    15,    42,    43,     0,    44,
     0,     0,    35,     0,     0,     0,     0,     0,     0,    70,
    72,    51,   123,   126,    45,     0,   197,    36,    38,    39,
    57,    59,    68,     0,    71 };
typedef struct
#ifdef __cplusplus
	yytoktype
#endif
{ char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"EVALEX",	257,
	"SCRIPT",	258,
	"=",	61,
	"COCO",	259,
	"INFIXL",	260,
	"INFIXR",	261,
	"INFIX",	262,
	"FUNARROW",	263,
	"-",	45,
	",",	44,
	"@",	64,
	"(",	40,
	")",	41,
	"|",	124,
	";",	59,
	"UPTO",	264,
	"[",	91,
	"]",	93,
	"CASEXP",	265,
	"OF",	266,
	"IF",	267,
	"THEN",	268,
	"ELSE",	269,
	"WHERE",	270,
	"TYPE",	271,
	"DATA",	272,
	"FROM",	273,
	"\\",	92,
	"~",	126,
	"LET",	274,
	"IN",	275,
	"`",	96,
	"VAROP",	276,
	"VARID",	277,
	"NUMLIT",	278,
	"CHARLIT",	279,
	"STRINGLIT",	280,
	"REPEAT",	281,
	"CONOP",	282,
	"CONID",	283,
	"TCLASS",	284,
	"IMPLIES",	285,
	"TINSTANCE",	286,
	"DO",	287,
	"TRUNST",	288,
	"PRIMITIVE",	289,
	"DEFAULT",	290,
	"DERIVING",	291,
	"HIDING",	292,
	"IMPORT",	293,
	"INTERFACE",	294,
	"MODULE",	295,
	"RENAMING",	296,
	"TO",	297,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"start : EVALEX exp",
	"start : EVALEX exp wherePart",
	"start : SCRIPT topModule",
	"start : error",
	"topModule : begin topDecls close",
	"topModule : modules",
	"begin : error",
	"topDecls : topDecls ';' topDecl",
	"topDecls : topDecls ';' decl",
	"topDecls : topDecl",
	"topDecls : decl",
	"topDecls : error",
	"modules : modules module",
	"modules : module",
	"module : MODULE modid expspec WHERE '{' topDecls close",
	"module : MODULE error",
	"topDecl : IMPORT modid impspec rename",
	"topDecl : IMPORT error",
	"modid : CONID",
	"modid : STRINGLIT",
	"expspec : /* empty */",
	"expspec : '(' exports ')'",
	"exports : exports ',' export",
	"exports : export",
	"export : entity",
	"export : modid UPTO",
	"impspec : /* empty */",
	"impspec : HIDING '(' imports ')'",
	"impspec : '(' imports0 ')'",
	"imports0 : /* empty */",
	"imports0 : imports",
	"imports : imports ',' entity",
	"imports : entity",
	"rename : /* empty */",
	"rename : RENAMING '(' renamings ')'",
	"renamings : renamings ',' renaming",
	"renamings : renaming",
	"renaming : var TO var",
	"renaming : conid TO conid",
	"entity : var",
	"entity : CONID",
	"entity : CONID '(' UPTO ')'",
	"entity : CONID '(' conids ')'",
	"entity : CONID '(' vars0 ')'",
	"conids : conids ',' conid",
	"conids : conid",
	"vars0 : /* empty */",
	"vars0 : vars",
	"topDecl : TYPE tyLhs '=' type invars",
	"topDecl : DATA type '=' constrs deriving",
	"topDecl : DATA context IMPLIES tyLhs '=' constrs deriving",
	"tyLhs : tyLhs VARID",
	"tyLhs : CONID",
	"tyLhs : error",
	"invars : IN rsvars",
	"invars : /* empty */",
	"rsvars : rsvars ',' rsvar",
	"rsvars : rsvar",
	"rsvar : var COCO sigType",
	"rsvar : var",
	"constrs : constrs '|' constr",
	"constrs : constr",
	"constr : type conop type",
	"constr : type",
	"constr : error",
	"deriving : /* empty */",
	"deriving : DERIVING CONID",
	"deriving : DERIVING '(' derivs0 ')'",
	"derivs0 : /* empty */",
	"derivs0 : derivs",
	"derivs : derivs ',' CONID",
	"derivs : CONID",
	"sigType : context IMPLIES type",
	"sigType : type",
	"context : type",
	"type : ctype",
	"type : ctype FUNARROW type",
	"type : error",
	"ctype : ctype atype",
	"ctype : atype",
	"atype : VARID",
	"atype : CONID",
	"atype : '(' ')'",
	"atype : '(' FUNARROW ')'",
	"atype : '(' type ')'",
	"atype : '(' ctype FUNARROW ')'",
	"atype : '(' tupCommas ')'",
	"atype : '(' typeTuple ')'",
	"atype : '[' type ']'",
	"atype : '[' ']'",
	"tupCommas : tupCommas ','",
	"tupCommas : ','",
	"typeTuple : typeTuple ',' type",
	"typeTuple : type ',' type",
	"topDecl : INFIXL optdigit ops",
	"topDecl : INFIXR optdigit ops",
	"topDecl : INFIX optdigit ops",
	"optdigit : NUMLIT",
	"optdigit : /* empty */",
	"ops : ops ',' op",
	"ops : op",
	"op : varop",
	"op : conop",
	"op : '-'",
	"varop : VAROP",
	"varop : '`' VARID '`'",
	"conop : CONOP",
	"conop : '`' CONID '`'",
	"topDecl : PRIMITIVE prims COCO sigType",
	"prims : prims ',' prim",
	"prims : prim",
	"prims : error",
	"prim : var STRINGLIT",
	"topDecl : TCLASS classHead classBody",
	"topDecl : TINSTANCE classHead instBody",
	"topDecl : DEFAULT type",
	"classHead : context IMPLIES type",
	"classHead : type",
	"classBody : WHERE '{' csigdecls close",
	"classBody : /* empty */",
	"instBody : WHERE '{' decls close",
	"instBody : /* empty */",
	"csigdecls : csigdecls ';' csigdecl",
	"csigdecls : csigdecl",
	"csigdecl : decl",
	"csigdecl : error",
	"decl : vars COCO sigType",
	"decl : opExp rhs",
	"decls : decls ';' decl",
	"decls : decl",
	"rhs : rhs1 wherePart",
	"rhs : rhs1",
	"rhs : error",
	"rhs1 : '=' exp",
	"rhs1 : gdefs",
	"wherePart : WHERE '{' decls close",
	"gdefs : gdefs gdef",
	"gdefs : gdef",
	"gdef : '|' exp '=' exp",
	"gdef : '=' exp ',' IF exp",
	"gdef : '=' exp ',' exp",
	"vars : vars ',' var",
	"vars : var",
	"var : varid",
	"var : '(' '-' ')'",
	"varid : VARID",
	"varid : '(' VAROP ')'",
	"conid : CONID",
	"conid : '(' CONOP ')'",
	"exp : opExp COCO sigType",
	"exp : opExp",
	"exp : error",
	"opExp : pfxExp",
	"opExp : pfxExp op pfxExp",
	"opExp : opExp0",
	"opExp0 : opExp0 op pfxExp",
	"opExp0 : pfxExp op pfxExp op pfxExp",
	"pfxExp : '-' appExp",
	"pfxExp : '\\' pats FUNARROW exp",
	"pfxExp : LET '{' decls close IN exp",
	"pfxExp : IF exp THEN exp ELSE exp",
	"pfxExp : CASEXP exp OF '{' alts close",
	"pfxExp : appExp",
	"pats : pats atomic",
	"pats : atomic",
	"appExp : appExp atomic",
	"appExp : TRUNST atomic",
	"appExp : atomic",
	"atomic : var",
	"atomic : var '@' atomic",
	"atomic : '~' atomic",
	"atomic : '_'",
	"atomic : conid",
	"atomic : '(' ')'",
	"atomic : NUMLIT",
	"atomic : CHARLIT",
	"atomic : STRINGLIT",
	"atomic : REPEAT",
	"atomic : '(' exp ')'",
	"atomic : '(' exps2 ')'",
	"atomic : '[' list ']'",
	"atomic : '(' pfxExp op ')'",
	"atomic : '(' varop atomic ')'",
	"atomic : '(' conop atomic ')'",
	"exps2 : exps2 ',' exp",
	"exps2 : exp ',' exp",
	"alts : alts ';' alt",
	"alts : alt",
	"alt : opExp altRhs",
	"altRhs : altRhs1 wherePart",
	"altRhs : altRhs1",
	"altRhs1 : guardAlts",
	"altRhs1 : FUNARROW exp",
	"altRhs1 : error",
	"guardAlts : guardAlts guardAlt",
	"guardAlts : guardAlt",
	"guardAlt : '|' opExp FUNARROW exp",
	"list : /* empty */",
	"list : exp",
	"list : exps2",
	"list : exp '|' quals",
	"list : exp UPTO exp",
	"list : exp ',' exp UPTO",
	"list : exp UPTO",
	"list : exp ',' exp UPTO exp",
	"quals : quals ',' qual",
	"quals : qual",
	"qual : exp FROM exp",
	"qual : exp '=' exp",
	"qual : exp",
	"qual : LET '{' decls close",
	"pfxExp : DO '{' dquals close1",
	"dqual : exp FROM exp",
	"dqual : LET '{' decls close",
	"dqual : IF exp",
	"dqual : exp",
	"dquals : dquals ';' dqual",
	"dquals : dqual",
	"close : ';' close1",
	"close : close1",
	"close1 : '}'",
	"close1 : error",
};
#endif /* YYDEBUG */
# line	1 "/usr/ccs/bin/yaccpar"
/*
 * Copyright (c) 1993 by Sun Microsystems, Inc.
 */

#pragma ident	"@(#)yaccpar	6.12	93/06/07 SMI"

/*
** Skeleton parser driver for yacc output
*/

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab
#define YYACCEPT	return(0)
#define YYABORT		return(1)
#define YYBACKUP( newtoken, newvalue )\
{\
	if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
	{\
		yyerror( "syntax error - cannot backup" );\
		goto yyerrlab;\
	}\
	yychar = newtoken;\
	yystate = *yyps;\
	yylval = newvalue;\
	goto yynewstate;\
}
#define YYRECOVERING()	(!!yyerrflag)
#define YYNEW(type)	malloc(sizeof(type) * yynewmax)
#define YYCOPY(to, from, type) \
	(type *) memcpy(to, (char *) from, yynewmax * sizeof(type))
#define YYENLARGE( from, type) \
	(type *) realloc((char *) from, yynewmax * sizeof(type))
#ifndef YYDEBUG
#	define YYDEBUG	1	/* make debugging available */
#endif

/*
** user known globals
*/
int yydebug;			/* set to 1 to get debugging */

/*
** driver internal defines
*/
#define YYFLAG		(-10000000)

/*
** global variables used by the parser
*/
YYSTYPE *yypv;			/* top of value stack */
int *yyps;			/* top of state stack */

int yystate;			/* current state */
int yytmp;			/* extra var (lasts between blocks) */

int yynerrs;			/* number of errors */
int yyerrflag;			/* error recovery flag */
int yychar;			/* current input token number */



#ifdef YYNMBCHARS
#define YYLEX()		yycvtok(yylex())
/*
** yycvtok - return a token if i is a wchar_t value that exceeds 255.
**	If i<255, i itself is the token.  If i>255 but the neither 
**	of the 30th or 31st bit is on, i is already a token.
*/
#if defined(__STDC__) || defined(__cplusplus)
int yycvtok(int i)
#else
int yycvtok(i) int i;
#endif
{
	int first = 0;
	int last = YYNMBCHARS - 1;
	int mid;
	wchar_t j;

	if(i&0x60000000){/*Must convert to a token. */
		if( yymbchars[last].character < i ){
			return i;/*Giving up*/
		}
		while ((last>=first)&&(first>=0)) {/*Binary search loop*/
			mid = (first+last)/2;
			j = yymbchars[mid].character;
			if( j==i ){/*Found*/ 
				return yymbchars[mid].tvalue;
			}else if( j<i ){
				first = mid + 1;
			}else{
				last = mid -1;
			}
		}
		/*No entry in the table.*/
		return i;/* Giving up.*/
	}else{/* i is already a token. */
		return i;
	}
}
#else/*!YYNMBCHARS*/
#define YYLEX()		yylex()
#endif/*!YYNMBCHARS*/

/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
*/
#if defined(__STDC__) || defined(__cplusplus)
int yyparse(void)
#else
int yyparse()
#endif
{
	register YYSTYPE *yypvt;	/* top of value stack for $vars */

#if defined(__cplusplus) || defined(lint)
/*
	hacks to please C++ and lint - goto's inside switch should never be
	executed; yypvt is set to 0 to avoid "used before set" warning.
*/
	static int __yaccpar_lint_hack__ = 0;
	switch (__yaccpar_lint_hack__)
	{
		case 1: goto yyerrlab;
		case 2: goto yynewstate;
	}
	yypvt = 0;
#endif

	/*
	** Initialize externals - yyparse may be called more than once
	*/
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

#if YYMAXDEPTH <= 0
	if (yymaxdepth <= 0)
	{
		if ((yymaxdepth = YYEXPAND(0)) <= 0)
		{
			yyerror("yacc initialization error");
			YYABORT;
		}
	}
#endif

	{
		register YYSTYPE *yy_pv;	/* top of value stack */
		register int *yy_ps;		/* top of state stack */
		register int yy_state;		/* current state */
		register int  yy_n;		/* internal state number info */
	goto yystack;	/* moved from 6 lines above to here to please C++ */

		/*
		** get globals into registers.
		** branch to here only if YYBACKUP was called.
		*/
	yynewstate:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;
		goto yy_newstate;

		/*
		** get globals into registers.
		** either we just started, or we just finished a reduction
		*/
	yystack:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;

		/*
		** top of for (;;) loop while no reductions done
		*/
	yy_stack:
		/*
		** put a state and value onto the stacks
		*/
#if YYDEBUG
		/*
		** if debugging, look up token value in list of value vs.
		** name pairs.  0 and negative (-1) are special values.
		** Note: linear search is used since time is not a real
		** consideration while debugging.
		*/
		if ( yydebug )
		{
			register int yy_i;

			printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ++yy_ps >= &yys[ yymaxdepth ] )	/* room on stack? */
		{
			/*
			** reallocate and recover.  Note that pointers
			** have to be reset, or bad things will happen
			*/
			int yyps_index = (yy_ps - yys);
			int yypv_index = (yy_pv - yyv);
			int yypvt_index = (yypvt - yyv);
			int yynewmax;
#ifdef YYEXPAND
			yynewmax = YYEXPAND(yymaxdepth);
#else
			yynewmax = 2 * yymaxdepth;	/* double table size */
			if (yymaxdepth == YYMAXDEPTH)	/* first time growth */
			{
				char *newyys = (char *)YYNEW(int);
				char *newyyv = (char *)YYNEW(YYSTYPE);
				if (newyys != 0 && newyyv != 0)
				{
					yys = YYCOPY(newyys, yys, int);
					yyv = YYCOPY(newyyv, yyv, YYSTYPE);
				}
				else
					yynewmax = 0;	/* failed */
			}
			else				/* not first time */
			{
				yys = YYENLARGE(yys, int);
				yyv = YYENLARGE(yyv, YYSTYPE);
				if (yys == 0 || yyv == 0)
					yynewmax = 0;	/* failed */
			}
#endif
			if (yynewmax <= yymaxdepth)	/* tables not expanded */
			{
				yyerror( "yacc stack overflow" );
				YYABORT;
			}
			yymaxdepth = yynewmax;

			yy_ps = yys + yyps_index;
			yy_pv = yyv + yypv_index;
			yypvt = yyv + yypvt_index;
		}
		*yy_ps = yy_state;
		*++yy_pv = yyval;

		/*
		** we have a new state - find out what to do
		*/
	yy_newstate:
		if ( ( yy_n = yypact[ yy_state ] ) <= YYFLAG )
			goto yydefault;		/* simple state */
#if YYDEBUG
		/*
		** if debugging, need to mark whether new token grabbed
		*/
		yytmp = yychar < 0;
#endif
		if ( ( yychar < 0 ) && ( ( yychar = YYLEX() ) < 0 ) )
			yychar = 0;		/* reached EOF */
#if YYDEBUG
		if ( yydebug && yytmp )
		{
			register int yy_i;

			printf( "Received token " );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ( ( yy_n += yychar ) < 0 ) || ( yy_n >= YYLAST ) )
			goto yydefault;
		if ( yychk[ yy_n = yyact[ yy_n ] ] == yychar )	/*valid shift*/
		{
			yychar = -1;
			yyval = yylval;
			yy_state = yy_n;
			if ( yyerrflag > 0 )
				yyerrflag--;
			goto yy_stack;
		}

	yydefault:
		if ( ( yy_n = yydef[ yy_state ] ) == -2 )
		{
#if YYDEBUG
			yytmp = yychar < 0;
#endif
			if ( ( yychar < 0 ) && ( ( yychar = YYLEX() ) < 0 ) )
				yychar = 0;		/* reached EOF */
#if YYDEBUG
			if ( yydebug && yytmp )
			{
				register int yy_i;

				printf( "Received token " );
				if ( yychar == 0 )
					printf( "end-of-file\n" );
				else if ( yychar < 0 )
					printf( "-none-\n" );
				else
				{
					for ( yy_i = 0;
						yytoks[yy_i].t_val >= 0;
						yy_i++ )
					{
						if ( yytoks[yy_i].t_val
							== yychar )
						{
							break;
						}
					}
					printf( "%s\n", yytoks[yy_i].t_name );
				}
			}
#endif /* YYDEBUG */
			/*
			** look through exception table
			*/
			{
				register int *yyxi = yyexca;

				while ( ( *yyxi != -1 ) ||
					( yyxi[1] != yy_state ) )
				{
					yyxi += 2;
				}
				while ( ( *(yyxi += 2) >= 0 ) &&
					( *yyxi != yychar ) )
					;
				if ( ( yy_n = yyxi[1] ) < 0 )
					YYACCEPT;
			}
		}

		/*
		** check for syntax error
		*/
		if ( yy_n == 0 )	/* have an error */
		{
			/* no worry about speed here! */
			switch ( yyerrflag )
			{
			case 0:		/* new error */
				yyerror( "syntax error" );
				goto skip_init;
			yyerrlab:
				/*
				** get globals into registers.
				** we have a user generated syntax type error
				*/
				yy_pv = yypv;
				yy_ps = yyps;
				yy_state = yystate;
			skip_init:
				yynerrs++;
				/* FALLTHRU */
			case 1:
			case 2:		/* incompletely recovered error */
					/* try again... */
				yyerrflag = 3;
				/*
				** find state where "error" is a legal
				** shift action
				*/
				while ( yy_ps >= yys )
				{
					yy_n = yypact[ *yy_ps ] + YYERRCODE;
					if ( yy_n >= 0 && yy_n < YYLAST &&
						yychk[yyact[yy_n]] == YYERRCODE)					{
						/*
						** simulate shift of "error"
						*/
						yy_state = yyact[ yy_n ];
						goto yy_stack;
					}
					/*
					** current state has no shift on
					** "error", pop stack
					*/
#if YYDEBUG
#	define _POP_ "Error recovery pops state %d, uncovers state %d\n"
					if ( yydebug )
						printf( _POP_, *yy_ps,
							yy_ps[-1] );
#	undef _POP_
#endif
					yy_ps--;
					yy_pv--;
				}
				/*
				** there is no state on stack with "error" as
				** a valid shift.  give up.
				*/
				YYABORT;
			case 3:		/* no shift yet; eat a token */
#if YYDEBUG
				/*
				** if debugging, look up token in list of
				** pairs.  0 and negative shouldn't occur,
				** but since timing doesn't matter when
				** debugging, it doesn't hurt to leave the
				** tests here.
				*/
				if ( yydebug )
				{
					register int yy_i;

					printf( "Error recovery discards " );
					if ( yychar == 0 )
						printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						printf( "token -none-\n" );
					else
					{
						for ( yy_i = 0;
							yytoks[yy_i].t_val >= 0;
							yy_i++ )
						{
							if ( yytoks[yy_i].t_val
								== yychar )
							{
								break;
							}
						}
						printf( "token %s\n",
							yytoks[yy_i].t_name );
					}
				}
#endif /* YYDEBUG */
				if ( yychar == 0 )	/* reached EOF. quit */
					YYABORT;
				yychar = -1;
				goto yy_newstate;
			}
		}/* end if ( yy_n == 0 ) */
		/*
		** reduction by production yy_n
		** put stack tops, etc. so things right after switch
		*/
#if YYDEBUG
		/*
		** if debugging, print the string that is the user's
		** specification of the reduction which is just about
		** to be done.
		*/
		if ( yydebug )
			printf( "Reduce by (%d) \"%s\"\n",
				yy_n, yyreds[ yy_n ] );
#endif
		yytmp = yy_n;			/* value to switch over */
		yypvt = yy_pv;			/* $vars top of value stack */
		/*
		** Look in goto table for next state
		** Sorry about using yy_state here as temporary
		** register variable, but why not, if it works...
		** If yyr2[ yy_n ] doesn't have the low order bit
		** set, then there is no action to be done for
		** this reduction.  So, no saving & unsaving of
		** registers done.  The only difference between the
		** code just after the if and the body of the if is
		** the goto yy_stack in the body.  This way the test
		** can be made before the choice of what to do is needed.
		*/
		{
			/* length of production doubled with extra bit */
			register int yy_len = yyr2[ yy_n ];

			if ( !( yy_len & 01 ) )
			{
				yy_len >>= 1;
				yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
				yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
					*( yy_ps -= yy_len ) + 1;
				if ( yy_state >= YYLAST ||
					yychk[ yy_state =
					yyact[ yy_state ] ] != -yy_n )
				{
					yy_state = yyact[ yypgo[ yy_n ] ];
				}
				goto yy_stack;
			}
			yy_len >>= 1;
			yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
			yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
				*( yy_ps -= yy_len ) + 1;
			if ( yy_state >= YYLAST ||
				yychk[ yy_state = yyact[ yy_state ] ] != -yy_n )
			{
				yy_state = yyact[ yypgo[ yy_n ] ];
			}
		}
					/* save until reenter driver code */
		yystate = yy_state;
		yyps = yy_ps;
		yypv = yy_pv;
	}
	/*
	** code supplied by user is placed in this switch
	*/
	switch( yytmp )
	{
		
case 1:
# line 86 "parser.y"
{inputExpr = yypvt[-0];	    sp-=1;} break;
case 2:
# line 87 "parser.y"
{inputExpr = letrec(yypvt[-0],yypvt[-1]); sp-=2;} break;
case 3:
# line 88 "parser.y"
{valDefns  = yypvt[-0];	    sp-=1;} break;
case 4:
# line 89 "parser.y"
{syntaxError("input");} break;
case 5:
# line 102 "parser.y"
{yyval = gc2(yypvt[-1]);} break;
case 6:
# line 103 "parser.y"
{yyval = yypvt[-0];} break;
case 7:
# line 105 "parser.y"
{yyerrok; goOffside(startColumn);} break;
case 8:
# line 107 "parser.y"
{yyval = gc2(yypvt[-2]);} break;
case 9:
# line 108 "parser.y"
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 10:
# line 109 "parser.y"
{yyval = gc0(NIL);} break;
case 11:
# line 110 "parser.y"
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 12:
# line 111 "parser.y"
{syntaxError("definition");} break;
case 13:
# line 113 "parser.y"
{yyval = gc2(appendOnto(yypvt[-0],yypvt[-1]));} break;
case 14:
# line 114 "parser.y"
{yyval = yypvt[-0];} break;
case 15:
# line 117 "parser.y"
{yyval = gc7(yypvt[-1]);} break;
case 16:
# line 118 "parser.y"
{syntaxError("module definition");} break;
case 17:
# line 120 "parser.y"
{sp-=4;} break;
case 18:
# line 121 "parser.y"
{syntaxError("import declaration");} break;
case 19:
# line 123 "parser.y"
{yyval = yypvt[-0];} break;
case 20:
# line 124 "parser.y"
{yyval = yypvt[-0];} break;
case 21:
# line 126 "parser.y"
{yyval = gc0(NIL);} break;
case 22:
# line 127 "parser.y"
{yyval = gc3(NIL);} break;
case 23:
# line 129 "parser.y"
{yyval = gc3(NIL);} break;
case 24:
# line 130 "parser.y"
{yyval = yypvt[-0];} break;
case 25:
# line 132 "parser.y"
{yyval = yypvt[-0];} break;
case 26:
# line 133 "parser.y"
{yyval = gc2(NIL);} break;
case 27:
# line 135 "parser.y"
{yyval = gc0(NIL);} break;
case 28:
# line 136 "parser.y"
{yyval = gc4(NIL);} break;
case 29:
# line 137 "parser.y"
{yyval = gc3(NIL);} break;
case 30:
# line 139 "parser.y"
{yyval = gc0(NIL);} break;
case 31:
# line 140 "parser.y"
{yyval = yypvt[-0];} break;
case 32:
# line 142 "parser.y"
{yyval = gc3(NIL);} break;
case 33:
# line 143 "parser.y"
{yyval = yypvt[-0];} break;
case 34:
# line 145 "parser.y"
{yyval = gc0(NIL);} break;
case 35:
# line 146 "parser.y"
{yyval = gc4(NIL);} break;
case 36:
# line 148 "parser.y"
{yyval = gc3(NIL);} break;
case 37:
# line 149 "parser.y"
{yyval = yypvt[-0];} break;
case 38:
# line 151 "parser.y"
{yyval = gc3(NIL);} break;
case 39:
# line 152 "parser.y"
{yyval = gc3(NIL);} break;
case 40:
# line 154 "parser.y"
{yyval = yypvt[-0];} break;
case 41:
# line 155 "parser.y"
{yyval = yypvt[-0];} break;
case 42:
# line 156 "parser.y"
{yyval = gc4(NIL);} break;
case 43:
# line 157 "parser.y"
{yyval = gc4(NIL);} break;
case 44:
# line 158 "parser.y"
{yyval = gc4(NIL);} break;
case 45:
# line 160 "parser.y"
{yyval = gc3(NIL);} break;
case 46:
# line 161 "parser.y"
{yyval = yypvt[-0];} break;
case 47:
# line 163 "parser.y"
{yyval = gc0(NIL);} break;
case 48:
# line 164 "parser.y"
{yyval = yypvt[-0];} break;
case 49:
# line 169 "parser.y"
{defTycon(5,yypvt[-2],yypvt[-3],yypvt[-1],yypvt[-0]);} break;
case 50:
# line 171 "parser.y"
{defTycon(5,yypvt[-2],checkTyLhs(yypvt[-3]),
							rev(yypvt[-1]),DATATYPE);} break;
case 51:
# line 174 "parser.y"
{defTycon(7,yypvt[-2],yypvt[-3],
						  ap(QUAL,pair(yypvt[-5],rev(yypvt[-1]))),
						  DATATYPE);} break;
case 52:
# line 178 "parser.y"
{yyval = gc2(ap(yypvt[-1],yypvt[-0]));} break;
case 53:
# line 179 "parser.y"
{yyval = yypvt[-0];} break;
case 54:
# line 180 "parser.y"
{syntaxError("type defn lhs");} break;
case 55:
# line 182 "parser.y"
{yyval = gc2(yypvt[-0]);} break;
case 56:
# line 183 "parser.y"
{yyval = gc0(SYNONYM);} break;
case 57:
# line 185 "parser.y"
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 58:
# line 186 "parser.y"
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 59:
# line 188 "parser.y"
{yyval = gc3(sigdecl(yypvt[-1],singleton(yypvt[-2]),
							     yypvt[-0]));} break;
case 60:
# line 190 "parser.y"
{yyval = yypvt[-0];} break;
case 61:
# line 192 "parser.y"
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 62:
# line 193 "parser.y"
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 63:
# line 195 "parser.y"
{yyval = gc3(ap(ap(yypvt[-1],yypvt[-2]),yypvt[-0]));} break;
case 64:
# line 196 "parser.y"
{if (!isCon(getHead(yypvt[-0])))
					     syntaxError("data constructor");
					 yyval = yypvt[-0];} break;
case 65:
# line 199 "parser.y"
{syntaxError("data type definition");} break;
case 66:
# line 201 "parser.y"
{yyval = gc0(NIL);} break;
case 67:
# line 202 "parser.y"
{yyval = gc2(singleton(yypvt[-0]));} break;
case 68:
# line 203 "parser.y"
{yyval = gc4(yypvt[-1]);} break;
case 69:
# line 205 "parser.y"
{yyval = gc0(NIL);} break;
case 70:
# line 206 "parser.y"
{yyval = yypvt[-0];} break;
case 71:
# line 208 "parser.y"
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 72:
# line 209 "parser.y"
{yyval = gc1(singleton(yypvt[-0]));} break;
case 73:
# line 220 "parser.y"
{yyval = gc3(ap(QUAL,pair(yypvt[-2],yypvt[-0])));} break;
case 74:
# line 221 "parser.y"
{yyval = yypvt[-0];} break;
case 75:
# line 223 "parser.y"
{yyval = gc1(checkContext(yypvt[-0]));} break;
case 76:
# line 225 "parser.y"
{yyval = yypvt[-0];} break;
case 77:
# line 226 "parser.y"
{yyval = gc3(ap(ap(ARROW,yypvt[-2]),yypvt[-0]));} break;
case 78:
# line 227 "parser.y"
{syntaxError("type expression");} break;
case 79:
# line 229 "parser.y"
{yyval = gc2(ap(yypvt[-1],yypvt[-0]));} break;
case 80:
# line 230 "parser.y"
{yyval = yypvt[-0];} break;
case 81:
# line 232 "parser.y"
{yyval = yypvt[-0];} break;
case 82:
# line 233 "parser.y"
{yyval = yypvt[-0];} break;
case 83:
# line 234 "parser.y"
{yyval = gc2(UNIT);} break;
case 84:
# line 235 "parser.y"
{yyval = gc3(ARROW);} break;
case 85:
# line 236 "parser.y"
{yyval = gc3(yypvt[-1]);} break;
case 86:
# line 237 "parser.y"
{yyval = gc4(ap(ARROW,yypvt[-2]));} break;
case 87:
# line 238 "parser.y"
{yyval = gc3(yypvt[-1]);} break;
case 88:
# line 239 "parser.y"
{yyval = gc3(buildTuple(yypvt[-1]));} break;
case 89:
# line 240 "parser.y"
{yyval = gc3(ap(LIST,yypvt[-1]));} break;
case 90:
# line 241 "parser.y"
{yyval = gc2(LIST);} break;
case 91:
# line 243 "parser.y"
{yyval = gc2(mkTuple(tupleOf(yypvt[-1])+1));} break;
case 92:
# line 244 "parser.y"
{yyval = gc1(mkTuple(2));} break;
case 93:
# line 246 "parser.y"
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 94:
# line 247 "parser.y"
{yyval = gc3(cons(yypvt[-0],cons(yypvt[-2],NIL)));} break;
case 95:
# line 252 "parser.y"
{fixDefn(LEFT_ASS,yypvt[-2],yypvt[-1],yypvt[-0]); sp-=3;} break;
case 96:
# line 253 "parser.y"
{fixDefn(RIGHT_ASS,yypvt[-2],yypvt[-1],yypvt[-0]);sp-=3;} break;
case 97:
# line 254 "parser.y"
{fixDefn(NON_ASS,yypvt[-2],yypvt[-1],yypvt[-0]);  sp-=3;} break;
case 98:
# line 256 "parser.y"
{yyval = gc1(checkPrec(yypvt[-0]));} break;
case 99:
# line 257 "parser.y"
{yyval = gc0(mkInt(DEF_PREC));} break;
case 100:
# line 259 "parser.y"
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 101:
# line 260 "parser.y"
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 102:
# line 262 "parser.y"
{yyval = yypvt[-0];} break;
case 103:
# line 263 "parser.y"
{yyval = yypvt[-0];} break;
case 104:
# line 264 "parser.y"
{yyval = gc1(varMinus);} break;
case 105:
# line 266 "parser.y"
{yyval = yypvt[-0];} break;
case 106:
# line 267 "parser.y"
{yyval = gc3(yypvt[-1]);} break;
case 107:
# line 269 "parser.y"
{yyval = yypvt[-0];} break;
case 108:
# line 270 "parser.y"
{yyval = gc3(yypvt[-1]);} break;
case 109:
# line 275 "parser.y"
{primDefn(yypvt[-3],yypvt[-2],yypvt[-0]); sp-=4;} break;
case 110:
# line 277 "parser.y"
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 111:
# line 278 "parser.y"
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 112:
# line 279 "parser.y"
{syntaxError("primitive defn");} break;
case 113:
# line 281 "parser.y"
{yyval = gc2(pair(yypvt[-1],yypvt[-0]));} break;
case 114:
# line 286 "parser.y"
{classDefn(intOf(yypvt[-2]),yypvt[-1],yypvt[-0]); sp-=3;} break;
case 115:
# line 287 "parser.y"
{instDefn(intOf(yypvt[-2]),yypvt[-1],yypvt[-0]);  sp-=3;} break;
case 116:
# line 288 "parser.y"
{sp-=2;} break;
case 117:
# line 290 "parser.y"
{yyval = gc3(pair(yypvt[-2],checkClass(yypvt[-0])));} break;
case 118:
# line 291 "parser.y"
{yyval = gc1(pair(NIL,checkClass(yypvt[-0])));} break;
case 119:
# line 293 "parser.y"
{yyval = gc4(yypvt[-1]);} break;
case 120:
# line 294 "parser.y"
{yyval = gc0(NIL);} break;
case 121:
# line 296 "parser.y"
{yyval = gc4(yypvt[-1]);} break;
case 122:
# line 297 "parser.y"
{yyval = gc0(NIL);} break;
case 123:
# line 299 "parser.y"
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 124:
# line 300 "parser.y"
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 125:
# line 302 "parser.y"
{yyval = gc1(yypvt[-0]);} break;
case 126:
# line 303 "parser.y"
{syntaxError("class body");} break;
case 127:
# line 308 "parser.y"
{yyval = gc3(sigdecl(yypvt[-1],yypvt[-2],yypvt[-0]));} break;
case 128:
# line 309 "parser.y"
{yyval = gc2(pair(yypvt[-1],yypvt[-0]));} break;
case 129:
# line 311 "parser.y"
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 130:
# line 312 "parser.y"
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 131:
# line 314 "parser.y"
{yyval = gc2(letrec(yypvt[-0],yypvt[-1]));} break;
case 132:
# line 315 "parser.y"
{yyval = yypvt[-0];} break;
case 133:
# line 316 "parser.y"
{syntaxError("declaration");} break;
case 134:
# line 318 "parser.y"
{yyval = gc2(pair(yypvt[-1],yypvt[-0]));} break;
case 135:
# line 319 "parser.y"
{yyval = gc1(grded(rev(yypvt[-0])));} break;
case 136:
# line 321 "parser.y"
{yyval = gc4(yypvt[-1]);} break;
case 137:
# line 323 "parser.y"
{yyval = gc2(cons(yypvt[-0],yypvt[-1]));} break;
case 138:
# line 324 "parser.y"
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 139:
# line 326 "parser.y"
{yyval = gc4(pair(yypvt[-1],pair(yypvt[-2],yypvt[-0])));} break;
case 140:
# line 333 "parser.y"
{yyval = gc5(pair(yypvt[-4],pair(yypvt[-0],yypvt[-3])));} break;
case 141:
# line 334 "parser.y"
{yyval = gc4(pair(yypvt[-3],pair(yypvt[-0],yypvt[-2])));} break;
case 142:
# line 336 "parser.y"
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 143:
# line 337 "parser.y"
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 144:
# line 339 "parser.y"
{yyval = yypvt[-0];} break;
case 145:
# line 340 "parser.y"
{yyval = gc3(varMinus);} break;
case 146:
# line 342 "parser.y"
{yyval = yypvt[-0];} break;
case 147:
# line 343 "parser.y"
{yyval = gc3(yypvt[-1]);} break;
case 148:
# line 345 "parser.y"
{yyval = yypvt[-0];} break;
case 149:
# line 346 "parser.y"
{yyval = gc3(yypvt[-1]);} break;
case 150:
# line 351 "parser.y"
{yyval = gc3(ap(ESIGN,pair(yypvt[-2],yypvt[-0])));} break;
case 151:
# line 352 "parser.y"
{yyval = yypvt[-0];} break;
case 152:
# line 353 "parser.y"
{syntaxError("expression");} break;
case 153:
# line 355 "parser.y"
{yyval = yypvt[-0];} break;
case 154:
# line 356 "parser.y"
{yyval = gc3(ap(ap(yypvt[-1],yypvt[-2]),yypvt[-0]));} break;
case 155:
# line 357 "parser.y"
{yyval = gc1(tidyInfix(yypvt[-0]));} break;
case 156:
# line 359 "parser.y"
{yyval = gc3(ap(ap(yypvt[-1],yypvt[-2]),yypvt[-0]));} break;
case 157:
# line 360 "parser.y"
{yyval = gc5(ap(ap(yypvt[-1],
							ap(ap(yypvt[-3],singleton(yypvt[-4])),
                                                           yypvt[-2])),yypvt[-0]));} break;
case 158:
# line 364 "parser.y"
{if (isInt(yypvt[-0]))
					     yyval = gc2(mkInt(-intOf(yypvt[-0])));
					 else
					     yyval = gc2(ap(varNegate,yypvt[-0]));
					} break;
case 159:
# line 369 "parser.y"
{yyval = gc4(ap(LAMBDA,
						     pair(rev(yypvt[-2]),
						          pair(yypvt[-1],yypvt[-0]))));} break;
case 160:
# line 372 "parser.y"
{yyval = gc6(letrec(yypvt[-3],yypvt[-0]));} break;
case 161:
# line 373 "parser.y"
{yyval = gc6(ap(COND,triple(yypvt[-4],yypvt[-2],yypvt[-0])));} break;
case 162:
# line 374 "parser.y"
{yyval = gc6(ap(CASE,pair(yypvt[-4],rev(yypvt[-1]))));} break;
case 163:
# line 375 "parser.y"
{yyval = yypvt[-0];} break;
case 164:
# line 377 "parser.y"
{yyval = gc2(cons(yypvt[-0],yypvt[-1]));} break;
case 165:
# line 378 "parser.y"
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 166:
# line 380 "parser.y"
{yyval = gc2(ap(yypvt[-1],yypvt[-0]));} break;
case 167:
# line 381 "parser.y"
{yyval = gc2(ap(RUNST,yypvt[-0]));} break;
case 168:
# line 382 "parser.y"
{yyval = yypvt[-0];} break;
case 169:
# line 384 "parser.y"
{yyval = yypvt[-0];} break;
case 170:
# line 385 "parser.y"
{yyval = gc3(ap(ASPAT,pair(yypvt[-2],yypvt[-0])));} break;
case 171:
# line 386 "parser.y"
{yyval = gc2(ap(LAZYPAT,yypvt[-0]));} break;
case 172:
# line 387 "parser.y"
{yyval = gc1(WILDCARD);} break;
case 173:
# line 388 "parser.y"
{yyval = yypvt[-0];} break;
case 174:
# line 389 "parser.y"
{yyval = gc2(UNIT);} break;
case 175:
# line 390 "parser.y"
{yyval = yypvt[-0];} break;
case 176:
# line 391 "parser.y"
{yyval = yypvt[-0];} break;
case 177:
# line 392 "parser.y"
{yyval = yypvt[-0];} break;
case 178:
# line 393 "parser.y"
{yyval = yypvt[-0];} break;
case 179:
# line 394 "parser.y"
{yyval = gc3(yypvt[-1]);} break;
case 180:
# line 395 "parser.y"
{yyval = gc3(buildTuple(yypvt[-1]));} break;
case 181:
# line 396 "parser.y"
{yyval = gc3(yypvt[-1]);} break;
case 182:
# line 397 "parser.y"
{yyval = gc4(ap(yypvt[-1],yypvt[-2]));} break;
case 183:
# line 398 "parser.y"
{yyval = gc4(ap(ap(varFlip,yypvt[-2]),yypvt[-1]));} break;
case 184:
# line 399 "parser.y"
{yyval = gc4(ap(ap(varFlip,yypvt[-2]),yypvt[-1]));} break;
case 185:
# line 401 "parser.y"
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 186:
# line 402 "parser.y"
{yyval = gc3(cons(yypvt[-0],cons(yypvt[-2],NIL)));} break;
case 187:
# line 404 "parser.y"
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 188:
# line 405 "parser.y"
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 189:
# line 407 "parser.y"
{yyval = gc2(pair(yypvt[-1],yypvt[-0]));} break;
case 190:
# line 409 "parser.y"
{yyval = gc2(letrec(yypvt[-0],yypvt[-1]));} break;
case 191:
# line 410 "parser.y"
{yyval = yypvt[-0];} break;
case 192:
# line 412 "parser.y"
{yyval = gc1(grded(rev(yypvt[-0])));} break;
case 193:
# line 413 "parser.y"
{yyval = gc2(pair(yypvt[-1],yypvt[-0]));} break;
case 194:
# line 414 "parser.y"
{syntaxError("case expression");} break;
case 195:
# line 416 "parser.y"
{yyval = gc2(cons(yypvt[-0],yypvt[-1]));} break;
case 196:
# line 417 "parser.y"
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 197:
# line 419 "parser.y"
{yyval = gc4(pair(yypvt[-1],pair(yypvt[-2],yypvt[-0])));} break;
case 198:
# line 424 "parser.y"
{yyval = gc0(nameNil);} break;
case 199:
# line 425 "parser.y"
{yyval = gc1(ap(FINLIST,cons(yypvt[-0],NIL)));} break;
case 200:
# line 426 "parser.y"
{yyval = gc1(ap(FINLIST,rev(yypvt[-0])));} break;
case 201:
# line 427 "parser.y"
{yyval = gc3(ap(COMP,pair(yypvt[-2],rev(yypvt[-0]))));} break;
case 202:
# line 428 "parser.y"
{yyval = gc3(ap(ap(varFromTo,yypvt[-2]),yypvt[-0]));} break;
case 203:
# line 429 "parser.y"
{yyval = gc4(ap(ap(varFromThen,yypvt[-3]),yypvt[-1]));} break;
case 204:
# line 430 "parser.y"
{yyval = gc2(ap(varFrom,yypvt[-1]));} break;
case 205:
# line 431 "parser.y"
{yyval = gc5(ap(ap(ap(varFromThenTo,
                                                               yypvt[-4]),yypvt[-2]),yypvt[-0]));} break;
case 206:
# line 434 "parser.y"
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 207:
# line 435 "parser.y"
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 208:
# line 437 "parser.y"
{yyval = gc3(ap(FROMQUAL,pair(yypvt[-2],yypvt[-0])));} break;
case 209:
# line 438 "parser.y"
{yyval = gc3(ap(QWHERE,
						     singleton(
							pair(yypvt[-2],pair(yypvt[-1],
								     yypvt[-0])))));} break;
case 210:
# line 442 "parser.y"
{yyval = gc1(ap(BOOLQUAL,yypvt[-0]));} break;
case 211:
# line 443 "parser.y"
{yyval = gc4(ap(QWHERE,yypvt[-1]));} break;
case 212:
# line 454 "parser.y"
{yyval = gc4(ap(DOCOMP,checkDo(yypvt[-1])));} break;
case 213:
# line 456 "parser.y"
{yyval = gc3(ap(FROMQUAL,pair(yypvt[-2],yypvt[-0])));} break;
case 214:
# line 457 "parser.y"
{yyval = gc4(ap(QWHERE,yypvt[-1]));} break;
case 215:
# line 458 "parser.y"
{yyval = gc2(ap(BOOLQUAL,yypvt[-0]));} break;
case 216:
# line 459 "parser.y"
{yyval = gc1(ap(DOQUAL,yypvt[-0]));} break;
case 217:
# line 461 "parser.y"
{yyval = gc3(cons(yypvt[-0],yypvt[-2]));} break;
case 218:
# line 462 "parser.y"
{yyval = gc1(cons(yypvt[-0],NIL));} break;
case 219:
# line 469 "parser.y"
{yyval = gc2(yypvt[-0]);} break;
case 220:
# line 470 "parser.y"
{yyval = yypvt[-0];} break;
case 221:
# line 472 "parser.y"
{yyval = yypvt[-0];} break;
case 222:
# line 473 "parser.y"
{yyerrok;
                                         if (canUnOffside()) {
                                             unOffside();
					     /* insert extra token on stack*/
					     push(NIL);
					     pushed(0) = pushed(1);
					     pushed(1) = mkInt(column);
					 }
                                         else
                                             syntaxError("definition");
                                        } break;
# line	532 "/usr/ccs/bin/yaccpar"
	}
	goto yystack;		/* reset registers in driver code */
}

