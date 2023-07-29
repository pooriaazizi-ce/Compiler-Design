# global variables
# for saving the state of the program
state = "start"
# for saving sequence of a digits
temp_digit = ""
# for saving sequence of an identifier
temp_id = ""
# for saving sequence of an invalid token
temp_invalid = ""

# main codes for lexer
def lexer(char_input):
    global state
    global temp_digit
    global temp_id
    global temp_invalid

# first char analysis
    if(state == "start"):
        if (char_input == "i"):
            state = "i"
        elif (char_input == "f"):
            state = "f"
        elif (char_input.isdigit()):
            state = "Digit"
            temp_digit = temp_digit+char_input
        elif (char_input.isidentifier()):
            state = "Identifier"
            temp_id = temp_id + char_input
        # (+) operand analysis
        elif (char_input == "+"):
            state = "Operand"
            lexer_resolt(state, "+")
            state = "start"
        elif (char_input == "<"):
            state = "<"
        elif (char_input == " "):
            state = "start"
# keyword analysis
# (if) keyword analysis
    elif(state == "i"):
        if(char_input == "f"):
            state = "if"
        else:
            temp_id = "i"
            if (temp_id.isidentifier()):
                state = "Identifier"
                lexer(char_input)
            else:
                temp_invalid = temp_id + char_input
                temp_id = ""
                state = "invalid"

    elif (state == "if"):
        if (char_input == " " or char_input == "(" or char_input == "\n" or char_input == ";"):
            state = "Keyword"
            lexer_resolt(state,"if")
            state = "start"
        else:
            temp_id = "if" + char_input
            if (temp_id.isidentifier()):
                state = "Identifier"
            else:
                temp_invalid = temp_id + char_input
                temp_id = ""
                state = "invalid"

# (for) keyword analysis
    elif (state == "f"):
        if (char_input == "o"):
            state = "fo"
        else:
            temp_id = "f"
            if (temp_id.isidentifier()):
                state = "Identifier"
                lexer(char_input)
            else:
                temp_invalid = temp_id + char_input
                temp_id = ""
                state = "invalid"

    elif (state == "fo"):
        if (char_input == "r"):
            state = "for"
        else:
            temp_id = "fo"
            if (temp_id.isidentifier()):
                state = "Identifier"
                lexer(char_input)
            else:
                temp_invalid = temp_id + char_input
                temp_id = ""
                state = "invalid"

    elif (state == "for"):
        if (char_input == " " or char_input == "\n" or char_input == ";"):
            state = "Keyword"
            lexer_resolt(state, "for")
            state = "start"
        else:
            temp_id = "for"
            if(temp_id.isidentifier()):
                state = "Identifier"
                lexer(char_input)
            else:
                temp_invalid = temp_id + char_input
                temp_id = ""
                state = "invalid"

# (<>) operand analysis
    elif (state == "<"):
        if (char_input == ">"):
            state = "Operand"
            lexer_resolt(state, "<>")
            state = "start"
        elif (char_input == "=" or char_input.isdigit()):
            state = "start"
        else:
            temp_invalid = "<" + char_input
            state = "invalid"

# digits analysis
    elif (state == "Digit"):
        if (char_input.isdigit()):
            state = "Digit"
            temp_digit = temp_digit + char_input
        elif (char_input == "+"):
            lexer_resolt(state, temp_digit)
            state = "Operand"
            lexer_resolt(state, "+")
            state = "start"
            temp_digit = ""
        elif (char_input == "<"):
            lexer_resolt(state, temp_digit)
            state = char_input
            temp_digit = ""
        elif (char_input == "-" or char_input == "/" or char_input == "*" or char_input == "<" or char_input == " "
              or char_input == ";" or char_input == ">" or char_input == ":" or char_input == "\n" or char_input == "="):
            lexer_resolt(state, temp_digit)
            state = "start"
            temp_digit = ""
        else:
            temp_invalid = temp_digit + char_input
            temp_digit = ""
            state = "invalid"

# Identifier analysis
    elif (state == "Identifier"):
        t = temp_id+char_input
        if (t.isidentifier()):
            state = "Identifier"
            temp_id = temp_id + char_input
        elif (char_input == "+"):
            lexer_resolt(state, temp_id)
            state = "Operand"
            lexer_resolt(state, "+")
            state = "start"
            temp_id = ""
        elif (char_input == "<"):
            lexer_resolt(state, temp_id)
            state = char_input
            temp_id = ""
        elif (char_input == "-" or char_input == "/" or char_input == "*" or char_input == " " or char_input == ">"
              or char_input == "<" or char_input == ":" or char_input == "\n" or char_input == ";" or char_input == "="):
            lexer_resolt(state, temp_id)
            state = "start"
            temp_id = ""
        else:
            temp_invalid = temp_id + char_input
            temp_id = ""
            state = "invalid"

# invalid token handling
    elif (state == "invalid") :
        if (char_input == " " or char_input == ";"):
            print(f'------- Invalid Token < "{temp_invalid}" >')
            state = "start"
            temp_invalid = ""
        else:
            temp_invalid = temp_invalid + char_input

# save and print tokens
def lexer_resolt(state,word):
    print(f'Token < "{word}" , "{state}" >')

# read the program from input file
def read_program(name):
    input_prog = open(name, 'r')
    while True:
        char_input = input_prog.read(1)
        if not char_input:
            # end of document
            lexer("\n")
            break
        lexer(char_input)
    input_prog.close()



read_program('test1.txt')