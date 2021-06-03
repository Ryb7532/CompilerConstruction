%%

%type Tokens.Token   // トークンの型
DIGITS=[0-9]+
KEYWORD="if"
STRING=[a-z][a-z0-9]*

%%

"if"                                 { Tokens.IF }
{STRING}                             { Tokens.ID(yytext()) }
"`"{KEYWORD}"`"|"`"{STRING}"`"       { Tokens.ID(yytext().substring(1, yytext().length-1))}
"0"{DIGITS}                          { Base.error() }
{DIGITS}                             { Tokens.NUM(yytext().toInt) }
{DIGITS}"."[0-9]*|[0-9]*"."{DIGITS}  { Tokens.REAL(yytext().toDouble) }
[\ \t\n]+                            { yylex() }
<<EOF>>                              { Tokens.EOF }
