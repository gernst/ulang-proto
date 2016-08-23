package ulang;

%%

%class Scanner
%function next
%type String
%unicode
%line
%column
%{
    private int commentNesting = 0;
    
    public int line()   { return yyline; }
    public int column() { return yycolumn; }
    public int pos()    { return yychar; }
    
    public String tok(String text) {
		return text;
	}
	
    public String tok() {
    	return tok(yytext());
    }
%}

nl = \r|\n|\r\n
sp = [ \t\f]
ws = {nl} | {sp} 

special = "[]" | "()" | "::" | "::="
id = [^ \r\n\t\f()\[\]\\.,:;\'\"]+
delim = [()\[\]\\.,:;]
string = [\'] [^\']* [\']

%state BLOCK_COMMENT
%state LINE_COMMENT

%%

<BLOCK_COMMENT> {

"/*"        { commentNesting ++; }
"*/"        { commentNesting --; if(commentNesting == 0) yybegin(YYINITIAL); }
{nl}        { /* ignore */ }
.           { /* ignore */ }
}

<LINE_COMMENT> {
{nl}        { yybegin(YYINITIAL); }
.           { /* ignore */ }
}

<YYINITIAL> {

"//"        { yybegin(LINE_COMMENT);  }
"/*"        { commentNesting = 1; yybegin(BLOCK_COMMENT); }

{ws}+    { /* ignore */ }

{delim}     { return tok(); }
{id}        { return tok(); }
{special}   { return tok(); }
{string}    { return tok(); }

// .|\n        { throw new RuntimeException("in scan: unexpected character '" + tok() + "'"); }
}