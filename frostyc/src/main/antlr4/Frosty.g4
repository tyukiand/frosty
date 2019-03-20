grammar Frosty;

entry: nsAst* EOF;

absolutePath: '`' ('/' ID)* '`';
relativePath: '`' ID ('/' ID)* '`';

importSubsel: 
    ID          # SubselectOne
  | ID '=>' '_' # IgnoreOne
  | '*'         # SubselectRest
  | ID '=>' ID  # SubselectRenameOne
  ;

importSel:
    '*'                                                     # SelectAll
  | ID                                                      # SelectOne
  | '{' importSubsel (',' importSubsel)* '}'                # SelectSubsels
  ;

importClause:
    'import' '`' ('/' ID)* '/' importSel '`'                # ImportAbs
  | 'import' '`' ID ('/' ID)* '/' importSel '`'             # ImportRel
  ;

visibilityModifier: keyword=(PUBLIC | PRIVATE) ;

nsAst:
    pkg
  | code
  | decl
  | underImport
  ;

underImport: importClause nsAst;
pkg: visibilityModifier 'package' '`' ID '`' '{' nsAst* '}';
decl: visibilityModifier '`' ID '`' ':' typ;
code: proc;

proc: 
    par                                                     # ParProc
  | proc '!' '(' sendable (',' sendable)* ')'               # SendProc
  | proc '?' arglist proc                                   # ReceiveProc
  | proc '(' sendable (',' sendable)* ')'                   # Invocation
  | '~' proc                                                # Unfreeze
  | '#' proc                                                # Freeze
  | '(' proc ')'                                            # Parens
  | '(' proc ':' typ ')'                                    # Ascription

  | 'new' nametypelist proc                                 # NewProc
  | contract                                                # ContractProc
  | funcdef                                                 # DefProc
  | 'await' proc                                            # AwaitProc

  | '()'                                                    # Unit

  | TRUE                                                    # True
  | FALSE                                                   # False
  | 'if' proc 'then' proc 'else' proc                       # IfElse
  | proc EQ proc                                            # Eq
  | NOT proc                                                # Not
  | proc AND proc                                           # And
  | proc OR proc                                            # Or

  | INT                                                     # Int
  | proc op=(MUL|DIV|REM) proc                              # MulDivRem
  | proc op=(ADD|SUB) proc                                  # AddSub
  | '(' '-' proc ')'                                        # Neg

  | STR_LIT                                                 # String
  | absolutePath                                            # AbsoluteName
  | relativePath                                            # RelativeName
  | ID                                                      # Var
  ;

// Parallel processes without curly braces.
pars: (proc ('|' proc)*)?;
// parallel processes enclosed in curly braces. A "block".
par: '{' pars '}';

// stuff enclosed between parens in a send.
sendable: 
    proc                                                    # SendableProc
    // allow omit curly braces in send
  | pars                                                    # SendablePars 
  ;

arg: ID;
arglist: '(' (arg (',' arg)*)? ')';

contract: 'contract' proc arglist proc;
funcdef:  'function' proc arglist '=' proc;

typarglist: '[' typ (',' typ)* ']';
typ:
    'Unit'                                                  # TypeUnit
  | 'Boolean'                                               # TypeBool
  | 'Int'                                                   # TypeInt
  | 'String'                                                # TypeString
  | 'Chan' typarglist                                       # TypeChan
  | 'Proc'                                                  # TypeFrozenProc
  | 'Run' typarglist                                        # TypeRun
  | 'Fun' typarglist                                        # TypeFunction
  ;

nametype: ID ':' typ;
nametypelist: nametype (',' nametype)*;

MUL: '*';
DIV: '/';
ADD: '+';
SUB: '-';
REM: '%';
AND: 'and';
OR:  'or';
NOT: 'not';
EQ: '==';
PUBLIC: 'public';
PRIVATE: 'private';

TRUE: 'true';
FALSE: 'false';
// No backslashes or quotes inside allowed,
// \" is an escaped quote,
// \n is allowed (line break),
// \\ is a backslash escape sequence
STR_LIT: '"' (~[\\"]|'\\"'|'\\n'|'\\\\')* '"';
INT: '0' | ([1-9][0-9]*);
ID: [a-z_][a-zA-Z0-9_]*;
TYP_ID: [A-Z][a-zA-Z0-9_]*;
// PATH_LIT: '`' PATH_FRAGMENT ('/' PATH_FRAGMENT)* '`';
WS: [ \t]+ -> skip;
NL: [\r\n]+ -> skip;
BLOCK_COMMENT: '/**' .*? '*/' -> skip;

// fragment PATH_FRAGMENT: [a-zA-Z][a-zA-Z0-9_]* ;

// How about this:
// internal f: (Int, Int) => Int
// function f(x, y) = x + y
// external
// contract
// runnable 