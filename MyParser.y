--PARSER
--IMPORTED MODULES
{	
module	MyParser where	
import	MyLexer	
}

%name	parser       	
%tokentype	{Token}	
%error	{parseError}

--TOKENS USED IN THE GRAMMAR AND USED FOR GENERATING ABSTRACT SYNTAX TREE

%token	
				choose   { TokenChoose _ }		
				from     { TokenFrom _ }	
			    suchThat { TokenSuchThat _}
			    orderBy  { TokenOrderBy _}
			    and      {TokenAnd _}
				aswell   {TokenAs _}
			    var      { TokenVar _ $$}
			    dig		 { TokenInt _ $$}
				'='	     { TokenEq _ }	
                ';'      { TokenEOS _ }
                '.'      {TokenDot _}
                ','      {TokenComma _}

--DEFINING THE ASSOCIATIVITY
%nonassoc ';'			
%nonassoc '='
%nonassoc '.'
%nonassoc ','
%left and
%left aswell
%left orderBy
%left suchThat
%left from
%left var
%left dig
%left choose	
		
%%

--THE GRAMMAR
--THE QUERY DATATYPE SYNTAX FOR RUNNING THE MAIN QUERY 

Query : choose Col from Table ';' {Choose $2 $4}
     | choose Col from Table Exp ';' {MChoose $2 $4 $5}
     | choose Col from Table Order ';' {OChoose $2 $4 $5}
     | choose Col from Table Exp aswell Order ';' {OMChoose $2 $4 $5 $7}

--THE COLUMN DATATYPE SYNTAX FOR THE COLUMNS

Col : var '.' dig {SCol $1 $3}
     | var '.' dig ',' Col {MCol $1 $3 $5}

--THE TABLE DATATYPE SYNTAX FOR THE TABLES    

Table : var {STab $1}
       | var and Table {MTab $1 $3}

--THE EXP DATATYPE SYNTAX FOR THE CLAUSES

Exp : var '.' dig '=' Col {ClauseEquality $1 $3 $5}
     |suchThat var '.' dig '=' Col {ClauseSuchThat $2 $4 $6}
     |suchThat var '.' dig '=' Col and Exp {ClauseMulSuchThat $2 $4 $6 $8}

--THE ORDER DATATYPE SYNTAX FOR ORDERINGBY CLAUSE     

Order : orderBy Col {ClauseOrderBy $2}

{	
parseError	::	[Token]	->	a
parseError	[]	=	error	"Parse error *** Empty File"	

--DEFINING ALL THE DATATYPES

data Query = Choose Col Table | MChoose Col Table Exp | OChoose Col Table Order | OMChoose Col Table Exp Order deriving (Show,Eq)
data Col =  SCol String Int | MCol String Int Col deriving (Show,Eq)
data Table = STab String | MTab String Table deriving (Show,Eq)
data Exp = ClauseEquality String Int Col | ClauseSuchThat String Int Col | ClauseMulSuchThat String Int Col Exp deriving (Eq,Show)
data Order = ClauseOrderBy Col deriving (Eq,Show)

}	