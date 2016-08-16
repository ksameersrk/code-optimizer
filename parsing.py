import io
import re
import tkinter
from itertools import count
import ply.lex as lex
import ply.yacc as yacc

counter = count(1)

class Node:
    def __init__(self, type, children):
        self.type = type
        self.children = children

tokens = (
    'NAME', 'NUMBER',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'ASSIGN',
    'LPAREN', 'RPAREN', 'NEWLINE'
    )

# Tokens
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_ASSIGN = r'\='
t_NAME = r'[a-zA-Z_][a-zA-Z0-9_]*'
t_NEWLINE = r';'


def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

# Ignored characters
t_ignore = " \t"


def t_error(t):
    # print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
lex.lex()

# Precedence rules for the arithmetic operators
precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'UMINUS'),
    )

# dictionary of names (for storing variables)
names = {}



def p_statement_expr(p):
    '''statement : expression'''
    p[0] = Node("expression", p[1])


def p_statement_assign(p):
    '''statement : NAME ASSIGN expression'''
    p[0] = Node("assignment", [p[1], p[3]])


def p_expression_binop(p):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression'''
    p[0] = Node("binop", [p[1], p[2], p[3]])


def p_expression_uminus(p):
    'expression : MINUS expression %prec UMINUS'
    p[0] = Node("uminus", [p[2]])


def p_expression_group(p):
    'expression : LPAREN expression RPAREN'
    p[0] = p[2]


def p_expression_number(p):
    'expression : NUMBER'
    p[0] = Node("number", p[1])


def p_expression_name(p):
    'expression : NAME'
    p[0] = Node("name", p[1])


def p_error(p):
    print("Syntax error at '%s'" % p.value)


class ASTNode:
    def __init__(self, operator, children):
        self.operator = operator
        self.children = children


def get_ast(root):
    if root.type in ('number', 'name'):
        return ASTNode(None, root.children)
    if root.type == 'binop':
        children = []
        children.append(get_ast(root.children[0]))
        children.append(get_ast(root.children[2]))
        children = tuple(children)
        return ASTNode(root.children[1], children)
    if root.type == 'assignment':
        children = get_ast(root.children[1])
        return ASTNode('=', (root.children[0], children))
    if root.type == 'expression':
        return get_ast(root.children)
    if root.type == 'uminus':
        return ASTNode('-', (get_ast(root.children[0]),))


class ASTWalk:
    def __init__(self, ast, optimize, file_):
        self.ast = ast
        self.hash_table = {}
        self.optimize = optimize
        self.file_ = file_

    def generate_code(self, ast=None):
        if not self.optimize:
            self.hash_table = {}
        if ast is None:
            ast = self.ast
        if ast in self.hash_table:
            return self.hash_table[ast]
        if ast.operator is None:
            if type(ast.children) == str:
                return '{}'.format(ast.children)
            if type(ast.children) == int:
                return ast.children
        elif ast.operator == '-' and len(ast.children) == 1:
            op1 = self.generate_code(ast.children[0])
            temp = '-{}'.format(op1)
            if temp in self.hash_table:
                return self.hash_table[temp]
            else:
                self.counter = next(counter)
                self.hash_table[temp] = 't{}'.format(self.counter)
                print('t{} = -{}'.format(self.counter, op1),
                      file=self.file_)
            return 't{}'.format(self.counter)
        elif ast.operator == '=':
            print('{} = {}'.format(ast.children[0],
                                   self.generate_code(ast.children[1])),
                  file=self.file_)
        else:
            op1 = self.generate_code(ast.children[0])
            op2 = self.generate_code(ast.children[1])
            rhs = '{} {} {}'.format(op1, ast.operator, op2)
            if rhs in self.hash_table:
                return 't{}'.format(self.hash_table[rhs])
            else:
                self.counter = next(counter)
                self.hash_table[rhs] = self.counter
                if ast.operator in ('+', '*'):
                    rhs = '{} {} {}'.format(op2, ast.operator, op1)
                    self.hash_table[rhs] = self.counter
                print('t{} = {} {} {}'.format(self.counter, op1,
                                              ast.operator, op2),
                      file=self.file_)
                return 't{}'.format(self.counter)


def _dead_code_elimination(s):

    expressions = [i for i in s.split('\n') if i]
    last_expr = expressions[-1]
    visited = [False for i in range(len(expressions)-1)]
    visited.append(True)
    queue = []
    rhs = last_expr.split('=')[1]
    queue.extend(re.findall(r'[A-Za-z][A-Za-z0-9]*',rhs))

    while queue:
        curr_var = queue.pop(0).strip()
        stmts = []
        for i in range(len(visited)):
            x = expressions[i].split("=")
            if not visited[i] and x[0].strip() == curr_var:
                stmts.append(i)
        
        if stmts:        
            index = sorted(stmts,reverse = True)[0]
            visited[index] = True
            rhs = expressions[index].split("=")[1]
            queue.extend(re.findall(r'[A-Za-z][A-Za-z0-9]*',rhs))

    tmp = []
    for j in [i for i in range(len(visited)) if visited[i]]:
        tmp.append(expressions[j])

    return '\n'.join(tmp)


def process():
    global counter
    yacc.yacc()
    naive = io.StringIO()
    cse_eliminated = io.StringIO()
    cse_eliminated_dead_eliminated = io.StringIO()
    options = [(naive, False, False),
               (cse_eliminated, True, False),
               (cse_eliminated_dead_eliminated, True, False)]

    for file_, cse, dce in options:
        counter = count(1)
        code = text1.get('1.0', tkinter.END).split('\n')
        code = [i for i in code if i.strip()]

        for line in code:
            i = yacc.parse(line+'\n')
            ast = get_ast(i)
            walk = ASTWalk(ast, optimize=cse, file_=file_)
            walk.generate_code()
        file_.seek(0)
    text2.delete('1.0', tkinter.END)
    text3.delete('1.0', tkinter.END)
    text4.delete('1.0', tkinter.END)
    text2.insert('1.0', naive.read())
    text3.insert('1.0', cse_eliminated.read())

    cse_eliminated.seek(0)
    text4.insert('1.0', _dead_code_elimination(cse_eliminated.read()))

if __name__ == '__main__':
    app = tkinter.Tk()
    frame1 = tkinter.Frame(app)
    label1 = tkinter.Label(frame1, text='source')
    text1 = tkinter.Text(frame1, width=50)

    frame2 = tkinter.Frame(app)
    label2 = tkinter.Label(frame2, text='naive')
    text2 = tkinter.Text(frame2, width=50)

    frame3 = tkinter.Frame(app)
    label3 = tkinter.Label(frame3, text='only cse')
    text3 = tkinter.Text(frame3, width=50)

    frame4 = tkinter.Frame(app)
    label4 = tkinter.Label(frame4, text='cse and dce')
    text4 = tkinter.Text(frame4, width=50)
    button = tkinter.Button(app, text='generate', command=process)

    button.pack()
    frame1.pack(side='left')
    frame2.pack(side='left')
    frame3.pack(side='left')
    frame4.pack(side='left')
    label1.pack()
    label2.pack()
    label3.pack()
    label4.pack()
    text1.pack()
    text2.pack()
    text3.pack()
    text4.pack()
    button.pack(side='bottom')
    app.mainloop()
