import ply.yacc as yacc
from ply_lex import tokens
import ply_lex

compile = True

def p_program(p): #!!!!
    '''program : C V P BEGIN statement S END SEMICOLON'''
def p_C(p):
    '''C : CONST const_decl C
    | epsilon'''
def p_V(p):
    '''V : VAR var_decl V
    | epsilon'''
def p_P(p):
    '''P : proc_decl P
    | epsilon'''
def p_S(p):
    '''S : statement S
    | epsilon'''

def p_const_decl(p): #!!!!
    '''const_decl : ID EQUAL integer SEMICOLON'''

def p_ID(p):
    '''ID : IDENTIFIER CAMMA ID
    | IDENTIFIER'''

def p_var_decl(p):
    '''var_decl : ID COLON type SEMICOLON'''

def p_type(p):
    '''type : INTEGER'''

def p_proc_decl(p):
    '''proc_decl : PROCEDURE IDENTIFIER LPAREN F RPAREN SEMICOLON block'''
def p_F(p):
    '''F : format SEMICOLON F
    | format
    | epsilon'''

def p_format(p):
    '''format : ID COLON M type'''
def p_M(p):
    '''M : mode
    | epsilon'''

def p_mode(p):
    '''mode : IN
    | OUT
    | IN OUT'''

def p_block(p): #!!!!
    '''block : BEGIN C V S END SEMICOLON'''

def p_statement(p):
    '''statement : block
    | print
    | read
    | asgn
    | cond
    | for
    | return
    | call'''

def p_print(p): #!!!!
    '''print : PRINT LPAREN string E RPAREN SEMICOLON'''
def p_E(p):
    '''E : expr CAMMA E
    | expr
    | epsilon'''

def p_read(p): #!!!!
    '''read : READ LPAREN string Va RPAREN SEMICOLON'''

def p_Va(p):
    '''Va : var CAMMA Va
    | expr
    | epsilon'''

def p_string(p):
    '''string : IDENTIFIER'''

def p_asgn(p):
    '''asgn : var ASSIGN expr SEMICOLON'''

def p_cond(p):
    '''cond : IF bool THEN statement EL'''
def p_EL(p):
    '''EL : ELSE statement
    | epsilon'''

def p_for(p):
    '''for : FOR IDENTIFIER ASSIGN expr TO expr DO statement'''

def p_return(p):
    '''return : RETURN SEMICOLON'''

def p_call(p):
    '''call : CALL IDENTIFIER LPAREN E RPAREN SEMICOLON'''

def p_expr(p): #!!!!
    '''expr : integer
    | MINES expr
    | expr PLUS expr
    | expr MINES expr
    | expr MUL expr
    | expr DIVIDE expr
    | LPAREN expr RPAREN
    | var'''

def p_var(p): #!!!!
    '''var : IDENTIFIER BRAC'''
def p_BRAC(p):
    '''BRAC : LBRACKET expr RBRACKET
    | epsilon'''

def p_bool(p): #!!!!
    '''bool : NOT bool
    | bool AND bool
    | bool OR bool
    | bool relop bool
    | LPAREN bool RPAREN
    | IDENTIFIER
    | DIGIT'''

def p_relop(p):
    '''relop : EQUAL
    | GREATER
    | LESS
    | GREATEQ
    | LESSEQ
    | NOT_EQUAL'''

def p_integer(p):
    '''integer : DIGIT'''

def p_epsilon(p):
    '''epsilon :'''

def p_error(p):
    global compile
    compile = False

    if(p != None):
        print(f"Syntax error at ({p.value})")
        print(p)
    else:
        print(f"Syntax error at (End of file)")


def parse_program(program):
    # lexer
    try:
        ply_lex.tokenize(program)
    except:
        print("lexer error")

    print("--------------------------------------------")

    # Build the Yacc
    parser = yacc.yacc()
    try:
        parser.parse(program)

        if (compile):
            print("Program Parsed Successfully")
    except:
        print("parse error")

# a program that includes all commands
pc ='''
const a=12;
const b=1;
var c: integer;
var d: integer;

procedure F (a:integer);begin
    print(a);
end;

procedure N (a:in integer; ff: out integer ;Ss:in out integer);
begin
    const num=5;
    const num1223=500;
    var ff: integer;
    var ff1223,w: integer;
    
    if (((not a) or (a <>num)) and (a>num)) then
        for i:=1 to a do print(i);
        read(num);
        ff :=45*i;
end;

begin
    call F(145);
    call N(45,45,45);
    B[3]:=123;
end;
'''

# reading the program from file and send it to parse_program
def read_program(input_file):
    try:
        f = open(input_file, 'r')
        program = f.read()
        parse_program(program)
        # parse_program(pc)
        f.close()
    except:
        raise Exception
        #print("File not found!")

read_program("test4.txt")

