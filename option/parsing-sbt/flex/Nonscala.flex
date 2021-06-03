%%

%type Tokens.Token

ALPHA=[A-Za-z]
DIGIT=[0-9]
WHITE_SPACE_CHAR=[\ \t\b\012]
BREAK=[;\n]

%%

"if"{BREAK}*     { Tokens.IF }
"else"{BREAK}*   { Tokens.ELSE }
"def"{BREAK}*    { Tokens.DEF }
"val"{BREAK}*    { Tokens.VAL }
"Nil"{BREAK}*    { Tokens.NIL }
"("{BREAK}*      { Tokens.LPAREN }
")"{BREAK}*      { Tokens.RPAREN }
"{"{BREAK}*      { Tokens.LCURLY }
"}"{BREAK}*      { Tokens.RCURLY }
"["{BREAK}*      { Tokens.LBRACKET }
"]"{BREAK}*      { Tokens.RBRACKET }
","{BREAK}*      { Tokens.COMMA }
"+"{BREAK}*      { Tokens.PLUS }
"-"{BREAK}*      { Tokens.MINUS }
"*"{BREAK}*      { Tokens.TIMES }
"/"{BREAK}*      { Tokens.DIV }
"."{BREAK}*      { Tokens.DOT }
":"{BREAK}*      { Tokens.COLON }
"::"{BREAK}*     { Tokens.COLONCOLON }
"="{BREAK}*      { Tokens.EQ }
"=="{BREAK}*     { Tokens.EQEQ }
"<"{BREAK}*      { Tokens.LESS }

{ALPHA}({ALPHA}|{DIGIT})*{BREAK} { Tokens.IDBR(yytext().substring(0, yytext().length-1)) }
{WHITE_SPACE_CHAR}        { yylex() }
{BREAK}                   { yylex() }
{ALPHA}({ALPHA}|{DIGIT})* { Tokens.ID(yytext()) }
{DIGIT}+                  { Tokens.INT(yytext().toInt) }
<<EOF>>                   { Tokens.EOF }