%%

%type Tokens.Token   // トークンの型
DIGITS=[0-9]+

%% 

"if"                                 { Tokens.IF }
[a-z][a-z0-9]*                       { Tokens.ID(yytext()) }
{DIGITS}                             { Tokens.NUM(yytext().toInt) }
{DIGITS}"."[0-9]*|[0-9]*"."{DIGITS}  { Tokens.REAL(yytext().toDouble) }
[\ \t\n]+                            { yylex() }
<<EOF>>                              { Tokens.EOF }
