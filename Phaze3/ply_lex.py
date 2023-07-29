import ply.lex as lex

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
    'DIGIT' , 'DIGIT_ERROR' , 'IDENTIFIER' ,
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

# tokenize the input program with ply lexer
def tokenize(program):
    # building the lexer and pass the input to it

    lexer.input(program)

    # print the tokens
    while True:
        token = lexer.token()
        if not token:  # end of tokens
            break
        print(token)