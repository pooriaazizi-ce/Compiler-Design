import ply.yacc as yacc
import ply.lex as lex

#------------------------------------LEXER---------------------------------

# List of token names
reserved = {
    # program
    'const' : 'CONST' , 'var' : 'VAR',
    'begin' : 'BEGIN' , 'end' : 'END',
    # simple-type
    'integer' : 'INTEGER' , 'char' : 'CHAR',
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
    'DIGIT' , 'DIGIT_ERROR' , 'IDENTIFIER' ,'STR',
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
    r'char'
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

def t_STR(t):
    r'\'[0-9a-zA-Z]+\''
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

def print_lexemes(p):
    lexer.input(p)
    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)


#------------------------------------PARSER-----------------------------------

# variable names
symbol_table = {}

# an array that save the semantic errors for report
errors = []

code_lines = []

compile = True

def p_program(p): #!!!!
    '''program : C V P BEGIN statement S END SEMICOLON'''
    code_lines.append(p[5])

def p_C(p):
    '''C : CONST const_decl C
    | epsilon'''
    if (len(p) == 4):
        p[0] = p[2]

def p_V(p):
    '''V : VAR var_decl V
    | epsilon'''
    if(len(p) == 4):
        p[0]=p[2]

def p_P(p):
    '''P : proc_decl P
    | epsilon'''
    if (len(p) == 3):
        p[0] = p[1]

def p_S(p):
    '''S : statement S
    | epsilon'''
    if(len(p)==3):
        code_lines.append(p[1])

def p_const_decl(p): #!!!!
    '''const_decl : ID EQUAL integer SEMICOLON'''

    # semantic analyze : put the given const variable into the symbol_table or create an error if this name is exist
    if p[1] in symbol_table.keys() and symbol_table.get(p[1])!='proc':
        errors.append("Semantic Error : Variable ("+p[1]+") is already defined!")
    else:
        symbol_table.__setitem__(p[1], 'integer')

def p_ID(p):
    '''ID : IDENTIFIER CAMMA ID
    | IDENTIFIER'''
    p[0] = p[1]

def p_var_decl(p):
    '''var_decl : ID COLON type SEMICOLON'''

    # semantic analyze : put the given variable into the symbol_table or create an error if this name is exist
    if p[1] in symbol_table.keys() and symbol_table.get(p[1])!='proc':
        errors.append("Semantic Error : Variable ("+p[1]+") is already defined!")
    else:
        symbol_table.__setitem__(p[1], p[3])

def p_type(p):
    '''type : INTEGER
    | CHAR'''
    p[0] = p[1]

def p_proc_decl(p):
    '''proc_decl : PROCEDURE IDENTIFIER LPAREN F RPAREN SEMICOLON block'''

    # semantic analyze : put the given proc into the symbol_table or create an error if this name is exist
    if p[2] in symbol_table.keys() and symbol_table.get(p[2]) == 'proc':
        errors.append("Semantic Error : Procedure (" + p[2] + ") is already defined!")
    else:
        symbol_table.__setitem__(p[2], 'proc')


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
    p[0]=p[1]

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

def p_asgn(p):
    '''asgn : var ASSIGN expr SEMICOLON'''

    # simple assign ( a := 3 )
    if not isinstance(p[3],list):
        # p[1] process for code generation
        if p[1] in symbol_table.keys() and ((symbol_table.get(p[1]) == 'integer' and isinstance(p[3],int))
                                            or (symbol_table.get(p[1]) == 'char' and isinstance(p[3],str))):
            # code generation : assign --> ( := , arg1 , result , )
            p[0]="("+p[2]+","+str(p[3])+","+p[1]+", )"

        # p[1] process for Type incompatibility in assign
        elif p[1] in symbol_table.keys() and ((symbol_table.get(p[1]) != 'integer' and isinstance(p[3],int))
                                              or (symbol_table.get(p[1]) != 'char' and isinstance(p[3],str)
                                              and p[3].startswith("'"))):
            # semantic analyze : Type incompatibility in assign
            errors.append("Semantic Error : Type incompatibility in assign: (" + p[1] + ") & (" + str(p[3]) + ")")

        if p[1] in symbol_table.keys() and p[3] in symbol_table.keys() and\
                ( (symbol_table.get(p[1]) == 'integer' and symbol_table.get(p[3]) == 'char') or
                (symbol_table.get(p[1]) == 'char' and symbol_table.get(p[3]) == 'integer') ):
            errors.append("Semantic Error : Type incompatibility in assign: (" + str(p[1]) + ") & (" + str(p[3]) + ")")

        # p[1] process for existing in symbol_table
        elif p[1] not in symbol_table.keys():
            # semantic analyze : Variable is not defined right side of assign statement
            errors.append("Semantic Error : Variable (" + p[1] + ") is not defined!")

        # p[3] process for existing in symbol_table if not char
        if(isinstance(p[3],str) and not p[3].startswith("'") and p[3] not in symbol_table.keys()):
            # semantic analyze : Variable is not defined left side of assign statement
            errors.append("Semantic Error : Variable (" + p[3] + ") is not defined!")

    # assign with an arithmetic instruction ( a := 3+b )
    else:
        # p[1] process for existing in symbol_table
        if p[1] not in symbol_table.keys():
            # semantic analyze : Variable is not defined right side of assign statement
            errors.append("Semantic Error : Variable (" + p[1] + ") is not defined!")

        # p[3] process --> mines instruction
        if(len(p[3])==2):
            # code generation : sign reverse --> ( - , arg1 , result , )
            p[0] = "(" + str(p[3][0]) + "," + str(p[3][1]) + "," + str(p[1]) + ", )"

            # p[3] process for existing in symbol_table if not char and number
            if (isinstance(p[3][1], str) and not p[3][1].startswith("'") and p[3][1] not in symbol_table.keys()):
                # semantic analyze : Variable is not defined left side of assign statement
                errors.append("Semantic Error : Variable (" + p[3][1] + ") is not defined!")

            # p[1] process for Type incompatibility in assign
            if p[1] in symbol_table.keys() and ((symbol_table.get(p[1]) != 'integer' and isinstance(p[3][1], int))
                                                or (symbol_table.get(p[1]) != 'char' and isinstance(p[3][1], str)
                                                    and p[3][1].startswith("'"))):
                # semantic analyze : Type incompatibility in assign
                errors.append("Semantic Error : Type incompatibility in assign: (" + p[1] + ") & (" + str(p[3][1]) + ")")

        # p[3] process --> other arithmetic instructions
        elif(len(p[3])==3 and not str(p[3][0]).startswith("(")):
            # code generation : arithmetic instructions --> ( operation , arg1 , arg2 , result )
            p[0] = "(" + str(p[3][1]) + "," + str(p[3][0]) + "," + str(p[3][2])+","+str(p[1]) + ")"

            # p[3][0] process for existing in symbol_table if not char and number
            if (isinstance(p[3][0], str) and not p[3][0].startswith("'") and p[3][0] not in symbol_table.keys()):
                # semantic analyze : Variable is not defined left side of assign statement
                errors.append("Semantic Error : Variable (" + p[3][0] + ") is not defined!")

            # p[3][2] process for existing in symbol_table if not char and number
            if (isinstance(p[3][2], str) and not p[3][2].startswith("'") and p[3][2] not in symbol_table.keys()):
                # semantic analyze : Variable is not defined left side of assign statement
                errors.append("Semantic Error : Variable (" + p[3][2] + ") is not defined!")

            # p[1] process for Type incompatibility in assign
            if p[1] in symbol_table.keys():
                if symbol_table.get(p[1]) != 'integer':

                        if(isinstance(p[3][0],int) or isinstance(p[3][2],int)) or\
                        (p[3][0] in symbol_table.keys() and symbol_table.get(p[3][0])=='integer') or \
                        (p[3][2] in symbol_table.keys() and symbol_table.get(p[3][2]) == 'integer'):
                            # semantic analyze : Type incompatibility in assign
                            errors.append("Semantic Error : Type incompatibility in assign: (" + p[1] +
                                ") & (" + str(p[3][0]) + ")"+ " & (" + str(p[3][2]) + ")")

                if symbol_table.get(p[1]) == 'integer':

                    if (isinstance(p[3][0],str) and p[3][0].startswith("'")) or\
                    (isinstance(p[3][2],str) and p[3][2].startswith("'")) or\
                    (p[3][0] in symbol_table.keys() and symbol_table.get(p[3][0]) == 'char') or\
                    (p[3][2] in symbol_table.keys() and symbol_table.get(p[3][2]) == 'char'):
                        # semantic analyze : Type incompatibility in assign
                        errors.append("Semantic Error : Type incompatibility in assign: (" + p[1] + ") & (" + str(p[3][0]) + ")"
                                      + " & (" + str(p[3][2]) + ")")

def p_cond(p):
    '''cond : IF bool THEN statement EL'''

    # code generation : if
    if (len(p[2]) == 2):
        if(p[5]!=None):
            code_lines.append("(" + str(p[2][0]) + "," + str(p[2][1]) + ",500, )"+
                              "\n(JPF,500, , )\n"+p[4]+"\n(JP, , , )\n"+p[5])
        else:
            code_lines.append("(" + str(p[2][0]) + "," + str(p[2][1]) + ", , )" +
                              "\n(JPF, , , )\n" + p[4])

    elif (len(p[2]) == 3):
        if (p[5] != None):
            code_lines.append("(" + str(p[2][1]) + "," + str(p[2][0]) + "," + str(p[2][2]) + ", )" +
                              "\n(JPF, , , )\n" + p[4] + "\n(JP, , , )\n" + p[5])
        else:
            code_lines.append("(" + str(p[2][1]) + "," + str(p[2][0]) + "," + str(p[2][2]) + ", )" +
                              "\n(JPF, , , )\n" + p[4])

def p_EL(p):
    '''EL : ELSE statement
    | epsilon'''
    if (len(p) == 3):
        p[0] = p[2]

def p_for(p):
    '''for : FOR ID ASSIGN expr TO expr DO statement'''

    # code generation : for
    code_lines.append("("+p[3]+","+str(p[4])+","+p[2]+", )\n"+
                      "(<,"+str(p[2]) + ", "+str(p[6])+", )"+
                      "\n(JPF, , , )\n"+
                      p[8]+
                      "\n(+,"+str(p[2]) + ",1,"+str(p[2]) + ")"+
                      "\n(JP, , , )")

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
    | string
    | ID'''
    if(len(p)==2):
        p[0]=p[1]
    elif(len(p)==3):
        p[0] = [p[1],p[2]]
    elif(len(p)==4):
        p[0] = [p[1], p[2],p[3]]

def p_var(p): #!!!!
    '''var : ID BRAC
    | ID'''
    p[0]=p[1]

def p_BRAC(p):
    '''BRAC : LBRACKET expr RBRACKET
    | epsilon'''

def p_bool(p): #!!!!
    '''bool : NOT bool
    | bool AND bool
    | bool OR bool
    | bool relop bool
    | LPAREN bool RPAREN
    | ID
    | integer'''
    if (len(p) == 2):
        p[0] = p[1]
    elif (len(p) == 3):
        p[0] = [p[1], p[2]]
    elif (len(p) == 4):
        p[0] = [p[1], p[2], p[3]]

def p_relop(p):
    '''relop : EQUAL
    | GREATER
    | LESS
    | GREATEQ
    | LESSEQ
    | NOT_EQUAL'''
    p[0] = p[1]

def p_integer(p):
    '''integer : DIGIT'''
    p[0] = p[1]

def p_string(p):
    '''string : STR'''
    p[0] = p[1]

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

        print("Lexeme tokens:")
        print_lexemes(program)
        lexer.lineno = 0

        print("-----------------------------")

        print("Intermediate code: (operation , arg1 , arg2 , result)")
        parser.parse(program)

        while (len(code_lines) != 0):
            l = code_lines.pop()
            if (l != None): print(l)

        print("-----------------------------")

        print("ST :")
        print(symbol_table)

        print("-----------------------------")

        if (compile and len(errors)==0):
            print("Program Parsed Successfully")
        elif(len(errors)!=0):
            print("Errors :")
            for i in errors:
                print(i)

    except Exception as e:
        print(e)

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

read_program("test5.txt")