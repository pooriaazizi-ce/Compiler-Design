import ply.yacc as yacc
import ply.lex as lex

#------------------------------------LEXER---------------------------------

# List of token names
reserved = {
    # program
    'const' : 'CONST' , 'var' : 'VAR',
    'begin' : 'BEGIN' , 'end' : 'END',
    # simple-type
    'integer' : 'INTEGER',
    # proc-decl
    'procedure' : 'PROCEDURE',
    # mode
    'in' : 'IN' , 'out' : 'OUT',
    # print
    'print' : 'PRINT',
    # read
    'read' : 'READ',
    # if
    'if' : 'IF' , 'then' : 'THEN' , 'else' : 'ELSE',
    # for
    'for' : 'FOR' , 'to' : 'TO' , 'do' : 'DO',
    # return
    'return' : 'RETURN',
    # call
    'call' : 'CALL',
    # bool
    'not' : 'NOT' , 'and' : 'AND' , 'or' : 'OR',
 }

tokens = [
    'DIGIT' , 'DIGIT_ERROR' , 'IDENTIFIER' ,'CHAR',
    'PLUS' , 'MINES' , 'DIVIDE' , 'MUL' , 'ASSIGN' ,
    'GREATER' , 'LESS' , 'GREATEQ' , 'LESSEQ' , 'NOT_EQUAL' , 'EQUAL' ,
    'LPAREN' , 'RPAREN' , 'LBRACKET' , 'RBRACKET' , 'SEMICOLON' , 'LACKO' , 'RACKO' , 'CAMMA','COLON',
    'ILLEGALCHAR'
] + list(reserved.values())

# Regular expression rules for simple tokens
# math operations ('+','-','/','*',':=')
t_PLUS = r'\+'
t_MINES = r'\-'
t_DIVIDE = r'\/'
t_MUL = r'\*'
t_ASSIGN = r'\:='
# comparison operations ('<','>','=','<=','>=','<>')
t_GREATER = r'>'
t_LESS = r'<'
t_GREATEQ = r'>='
t_LESSEQ  = r'<='
t_NOT_EQUAL = r'\<>'
t_EQUAL = r'\='
# signs ( '(' , ')' , '[' , ']' , '{' , '}' , ';' , ',' , ':')
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_SEMICOLON = r'\;'
t_LACKO = r'\{'
t_RACKO = r'\}'
t_CAMMA = r'\,'
t_COLON = r'\:'
# illegal chars ( '@','!' )
t_ILLEGALCHAR = r'@!'
# ignored characters (space and tab)
t_ignore = ' \t'

# Regular expression rules for reserved tokens
def t_CONST(t):
    r'const'
    return t
def t_VAR(t):
    r'var'
    return t
def t_BEGIN(t):
    r'begin'
    return t
def t_END(t):
    r'end'
    return t
def t_INTEGER(t):
    r'integer'
    return t
def t_CHAR(t):
    r'\'[0-9a-zA-Z]+\''
    return t
def t_PROCEDURE (t):
    r'procedure'
    return t
def t_IN(t):
    r'in'
    return t
def t_OUT(t):
    r'out'
    return t
def t_PRINT(t):
    r'print'
    return t
def t_READ(t):
    r'read'
    return t
def t_IF(t):
    r'if'
    return t
def t_THEN(t):
    r'then'
    return t
def t_ELSE(t):
    r'else'
    return t
def t_FOR(t):
    r'for'
    return t
def t_TO(t):
    r'to'
    return t
def t_DO(t):
    r'do'
    return t
def t_RETURN(t):
    r'return'
    return t
def t_CALL(t):
    r'call'
    return t
def t_AND(t):
    r'and'
    return t
def t_OR(t):
    r'or'
    return t
def t_NOT(t):
    r'not'
    return t

# Regular expression rules for complex tokens
def t_DIGIT_ERROR(t):
    r'[0-9]+[a-zA-Z_][a-zA-Z0-9_]*'
    print(f'Invalid Digit Token ---> ({t.value})')
    a = f'Invalid Digit Token ---> ({t.value})' + "\n"
    # return t

def t_DIGIT(t):
    r'[0-9]+'
    try:
        t.value = int(t.value)
    except ValueError:
        print("Integer value too large %d", t.value)
        t.value = 0
    return t

def t_IDENTIFIER(t):
    r'[a-zA-Z_][0-9a-zA-Z_]*'
    return t

# Define a rule so we can track line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Error handling rule
def t_error(t):
    print(f"Illegal character --->({t.value[0]})")
    a = f"Illegal character --->({t.value[0]})" + "\n"
    t.lexer.skip(1)

lexer = lex.lex()

#------------------------------------SEMANTIC---------------------------------

# an array that save the semantic errors for report
Semantic_error = {}

def sem_analize(program):

    lexer.input(program)

    # counter for (Semantic_error) array
    sem_count = 0

    # this array keeps the lexer tokens in this type ( LexToken(CHAR,"'chr'",3,32) )
    Tokens = {}
    counterTokens = 0

    # this array keeps the tokens in this type ( chr )
    T = {}
    counterT = 0

    # an array for keeping the variables after define
    symboltable = {}
    counterSymbol = 0

    # when program read an assign command in code and the variable is integer this flag turns true
    flag = False

    while True:
        # tokenize and save the tokens into Tokens array
        tok = lexer.token()
        if not tok:
            break
        Tokens[counterTokens] = tok.__str__()
        counterTokens+=1
        print(tok)

        # save the token itself in T array
        firstPos = tok.__str__().find("\'")
        firstPos+=1
        lastPos = tok.__str__().rfind("\'")
        result = tok.__str__()[firstPos:lastPos]
        T[counterT] = result
        counterT += 1

        # process of the semantic errors
        if (flag):
            # the integer tokens save into result by this format [ LexToken(DIGIT,... ]
            if not result.startswith("LexToken"):
                notdefine = True
                for i in symboltable:
                    if result == symboltable[i]:
                        notdefine = False
                        break
                if notdefine:
                    Semantic_error[sem_count] = Tokens[counterTokens-1]
                    sem_count +=1
            flag = False

        # saving the integer variables into symboltable array
        if result == "integer":
            c = counterT - 3
            symboltable[counterSymbol] = T[c]
            counterSymbol += 1
        # turn flag true by seeing :=
        elif result == ":=":
            c = counterT - 2
            for item in symboltable:
                if symboltable[item] == T[c]:
                    flag = True

#------------------------------------PARSER-----------------------------------

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
    '''string : IDENTIFIER
    | CHAR'''

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
    | var
    | CHAR'''

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
    # Build the Yacc
    parser = yacc.yacc()
    try:

        sem_analize(program)

        print("-----------------------------")

        parser.parse(program)

        if (compile and len(Semantic_error)==0):
            print("Program Parsed Successfully")
        elif(len(Semantic_error)!=0):
            print("Semantic Error : Type incompatibility in assign")
            for i in Semantic_error:
                print(Semantic_error[i])

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

read_program("test1.txt")
