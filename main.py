import sys

try:
    import readline
except ImportError:
    pass

from enum import Enum
from expr import *
from stmt import *

class LoxParseError(Exception):
    pass

class LoxRuntimeError(RuntimeError):
    def __init__(self, token, msg):
        self.token = token
        super().__init__(msg)

class TokenType(Enum):
    (
    LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
    COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,

    BANG, BANG_EQUAL,
    EQUAL, EQUAL_EQUAL,
    GREATER, GREATER_EQUAL,
    LESS, LESS_EQUAL,

    IDENTIFIER, STRING, NUMBER,

    AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
    PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,

    EOF
    ) = range(39)

class Token:
    def __init__(self, type_, lexeme, literal, line):
        self.type_ = type_
        self.lexeme = lexeme
        self.literal = literal
        self.line = line

class Lexer:
    keywords = {
        'and': TokenType.AND,
        'class': TokenType.CLASS,
        'else': TokenType.ELSE,
        'false': TokenType.FALSE,
        'for': TokenType.FOR,
        'fun': TokenType.FUN,
        'if': TokenType.IF,
        'nil': TokenType.NIL,
        'or': TokenType.OR,
        'print': TokenType.PRINT,
        'return': TokenType.RETURN,
        'super': TokenType.SUPER,
        'this': TokenType.THIS,
        'true': TokenType.TRUE,
        'var': TokenType.VAR,
        'while': TokenType.WHILE,
    }

    def __init__(self, src):
        self.src = src
        self.tokens = []
        self.line = 1
        self.start = 0
        self.current = 0

    def at_end(self):
        return self.current >= len(self.src)

    def advance(self):
        self.current += 1
        return self.src[self.current-1]

    def add_token(self, type_, literal=None):
        self.tokens.append(Token(type_, self.src[self.start:self.current], literal, self.line))

    def next(self, exp):
        if self.at_end() or self.src[self.current] != exp:
            return False
        self.current += 1
        return True

    def peek(self):
        if self.at_end():
            return '\0'
        return self.src[self.current]

    def peek_peek(self):
        if self.current+1 >= len(self.src):
            return '\0'
        return self.src[self.current+1]

    def is_digit(self, c):
        return '0' <= c <= '9'

    def is_alpha(self, c):
        return 'a' <= c <= 'z' or 'A' <= c <= 'Z' or c == '_'

    def is_alpha_numeric(self, c):
        return self.is_alpha(c) or self.is_digit(c)

    def consume_string(self):
        while self.peek() != '"' and not self.at_end():
            if self.peek() == '\n':
                self.line += 1
            self.advance()

        if self.at_end():
            Lox.error(self.line, 'unterminated string.')

        self.advance()
        self.add_token(TokenType.STRING, self.src[self.start+1:self.current-1])

    def consume_number(self):
        def consume_integer():
            while self.is_digit(self.peek()):
                self.advance()

        consume_integer()
        if self.peek() == '.' and self.is_digit(self.peek_peek()):
            self.advance()
            consume_integer()
        self.add_token(TokenType.NUMBER, float(self.src[self.start:self.current]))

    def consume_identifier(self):
        while self.is_alpha_numeric(self.peek()):
            self.advance()
        self.add_token(self.keywords.get(self.src[self.start:self.current], TokenType.IDENTIFIER))

    def lex_token(self):
        match self.advance():
            case '(': self.add_token(TokenType.LEFT_PAREN)
            case ')': self.add_token(TokenType.RIGHT_PAREN)
            case '{': self.add_token(TokenType.LEFT_BRACE)
            case '}': self.add_token(TokenType.RIGHT_BRACE)
            case ',': self.add_token(TokenType.COMMA)
            case '.': self.add_token(TokenType.DOT)
            case '-': self.add_token(TokenType.MINUS)
            case '+': self.add_token(TokenType.PLUS)
            case ';': self.add_token(TokenType.SEMICOLON)
            case '*': self.add_token(TokenType.STAR)
            case '!':
                self.add_token(TokenType.BANG_EQUAL if self.next('=') else TokenType.BANG)
            case '=':
                self.add_token(TokenType.EQUAL_EQUAL if self.next('=') else TokenType.EQUAL)
            case '<':
                self.add_token(TokenType.LESS_EQUAL if self.next('=') else TokenType.LESS)
            case '>':
                self.add_token(TokenType.GREATER_EQUAL if self.next('=') else TokenType.GREATER)
            case '/':
                if self.next('/'):
                    while self.peek() != '\n' and not self.at_end():
                        self.advance()
                else:
                    self.add_token(TokenType.SLASH)
            case '"':
                self.consume_string()
            case '\n':
                self.line += 1
            case ' '|'\r'|'\t':
                pass
            case c if self.is_digit(c):
                self.consume_number()
            case c if self.is_alpha(c):
                self.consume_identifier()
            case c:
                Lox.error(self.line, f'unexpected character "{c}".')

    def lex(self):
        while not self.at_end():
            self.start = self.current
            self.lex_token()

        self.tokens.append(Token(TokenType.EOF, '', None, self.line))
        return self.tokens

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.current = 0

    def parse(self):
        stmts = []
        while not self.at_end():
            stmts.append(self.declaration())
        return stmts

    def expression(self):
        return self.equality()

    def declaration(self):
        try:
            if self.next(TokenType.VAR):
                return self.var_declaration()
            return self.statement()
        except LoxParseError:
            self.synchronize()
            return None

    def statement(self):
        if self.next(TokenType.PRINT):
            return self.print_statement()
        return self.expression_statement()

    def print_statement(self):
        value = self.expression()
        self.consume(TokenType.SEMICOLON, 'expect ";" after value.')
        return Print(value)

    def var_declaration(self):
        name = self.consume(TokenType.IDENTIFIER, 'expect variable name.')

        initializer = None
        if self.next(TokenType.EQUAL):
            initializer = self.expression()

        self.consume(TokenType.SEMICOLON, 'expect ";" after variable declaration.')
        return Var(name, initializer)

    def expression_statement(self):
        expr = self.expression()
        self.consume(TokenType.SEMICOLON, 'expect ";" after expression.')
        return Expression(expr)

    def equality(self):
        expr = self.comparison()
        while self.next(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL):
            expr = Binary(expr, self.previous(), self.comparison())
        return expr

    def comparison(self):
        expr = self.term()
        while self.next(
            TokenType.GREATER,
            TokenType.GREATER_EQUAL,
            TokenType.LESS,
            TokenType.LESS_EQUAL
        ):
            expr = Binary(expr, self.previous(), self.term())
        return expr

    def term(self):
        expr = self.factor()
        while self.next(TokenType.MINUS, TokenType.PLUS):
            expr = Binary(expr, self.previous(), self.factor())
        return expr

    def factor(self):
        expr = self.unary()
        while self.next(TokenType.SLASH, TokenType.STAR):
            expr = Binary(expr, self.previous(), self.unary())
        return expr

    def unary(self):
        if self.next(TokenType.BANG, TokenType.MINUS):
            return Unary(self.previous(), self.unary())
        return self.primary()

    def primary(self):
        if self.next(TokenType.FALSE):
            return Literal(False)
        if self.next(TokenType.TRUE):
            return Literal(True)
        if self.next(TokenType.NIL):
            return Literal(None)

        if self.next(TokenType.NUMBER, TokenType.STRING):
            return Literal(self.previous().literal)

        if self.next(TokenType.IDENTIFIER):
            return Variable(self.previous())

        if self.next(TokenType.LEFT_PAREN):
            expr = self.expression()
            self.consume(TokenType.RIGHT_PAREN, 'unterminated parentheses.')
            return Grouping(expr)

        raise self.error(self.peek(), 'expected expression.')

    def next(self, *types):
        if any(self.check(t) for t in types):
            self.advance()
            return True
        return False

    def consume(self, type_, msg):
        if self.check(type_):
            return self.advance()
        raise self.error(self.peek(), msg)

    def check(self, type_):
        if self.at_end():
            return False
        return self.peek().type_ == type_

    def advance(self):
        if not self.at_end():
            self.current += 1
        return self.previous()

    def at_end(self):
        return self.peek().type_ == TokenType.EOF

    def peek(self):
        return self.tokens[self.current]

    def previous(self):
        return self.tokens[self.current-1]

    def error(self, token, msg):
        Lox.token_error(token, msg)
        return LoxParseError()

    def synchronize(self):
        self.advance()
        while not self.at_end():
            if self.previous().type_ == TokenType.SEMICOLON:
                return

            match self.peek().type_:
                case (
                    TokenType.CLASS|
                    TokenType.FUN|
                    TokenType.VAR|
                    TokenType.FOR|
                    TokenType.IF|
                    TokenType.WHILE|
                    TokenType.PRINT|
                    TokenType.RETURN
                ):
                    return

            self.advance()

class Environment:
    def __init__(self):
        self.values = {}

    def define(self, name, val):
        self.values[name] = val

    def get(self, name):
        if name.lexeme in self.values:
            return self.values[name.lexeme]
        raise LoxRuntimeError(name, f'undefined variable "{name.lexeme}".')

class Interpreter(ExprVisitor, StmtVisitor):
    def __init__(self):
        self.environment = Environment()

    def interpret(self, stmts):
        try:
            for stmt in stmts:
                self.execute(stmt)
        except LoxRuntimeError as e:
            Lox.runtime_error(e)

    def visit_assign_expr(self, expr):
        pass

    def visit_binary_expr(self, expr):
        left = self.evaluate(expr.left)
        right = self.evaluate(expr.right)

        match expr.operator.type_:
            case TokenType.GREATER:
                self.check_num_operands(expr.operator, left, right)
                return left > right
            case TokenType.GREATER_EQUAL:
                self.check_num_operands(expr.operator, left, right)
                return left >= right
            case TokenType.LESS:
                self.check_num_operands(expr.operator, left, right)
                return left < right
            case TokenType.LESS_EQUAL:
                self.check_num_operands(expr.operator, left, right)
                return left <= right
            case TokenType.BANG_EQUAL:
                return not self.is_equal(left, right)
            case TokenType.EQUAL_EQUAL:
                return self.is_equal(left, right)
            case TokenType.MINUS:
                self.check_num_operands(expr.operator, left, right)
                return left - right
            case TokenType.PLUS:
                if not isinstance(left, (float, str)) or not isinstance(right, type(left)):
                    raise LoxRuntimeError(expr.operator, 'operands must be numbers or strings.')
                return left + right
            case TokenType.SLASH:
                self.check_num_operands(expr.operator, left, right)
                if right == 0:
                    raise LoxRuntimeError(expr.operator, 'division by zero.')
                return left / right
            case TokenType.STAR:
                self.check_num_operands(expr.operator, left, right)
                return left * right
        return None

    def visit_call_expr(self, expr):
        pass

    def visit_get_expr(self, expr):
        pass

    def visit_grouping_expr(self, expr):
        return self.evaluate(expr.expression)

    def visit_literal_expr(self, expr):
        return expr.value

    def visit_logical_expr(self, expr):
        pass

    def visit_set_expr(self, expr):
        pass

    def visit_super_expr(self, expr):
        pass

    def visit_this_expr(self, expr):
        pass

    def visit_unary_expr(self, expr):
        right = self.evaluate(expr.right)
        match expr.operator.type_:
            case TokenType.BANG:
                return not self.is_truthy(right)
            case TokenType.MINUS:
                self.check_num_operand(expr.operator, right)
                return -right
        return None

    def visit_variable_expr(self, expr):
        return self.environment.get(expr.name)

    def check_num_operand(self, op, operand):
        if not isinstance(operand, float):
            raise LoxRuntimeError(op, 'operand must be a number.')

    def check_num_operands(self, op, left, right):
        if not isinstance(left, float) or not isinstance(right, float):
            raise LoxRuntimeError(op, 'operands must be numbers.')

    def is_truthy(self, obj):
        if isinstance(obj, bool):
            return obj
        return obj is None

    def is_equal(self, a, b):
        return a == b

    def stringify(self, obj):
        if obj is None:
            return 'nil'
        if isinstance(obj, float):
            return str(int(obj) if obj.is_integer() else obj)
        return str(obj)

    def evaluate(self, expr):
        return expr.accept(self)

    def execute(self, stmt):
        stmt.accept(self)

    def visit_expression_stmt(self, stmt):
        self.evaluate(stmt.expression)

    def visit_print_stmt(self, stmt):
        print(self.stringify(self.evaluate(stmt.expression)))

    def visit_var_stmt(self, stmt):
        val = None
        if stmt.initializer is not None:
            val = self.evaluate(stmt.initializer)
        self.environment.define(stmt.name.lexeme, val)
        return None

class Lox:
    interpreter = Interpreter()
    had_error = False
    had_runtime_error = False

    @staticmethod
    def error(line, msg):
        print(f'lox: line {line}: {msg}')
        Lox.had_error = True

    @staticmethod
    def token_error(token, msg):
        where = 'end' if token.type_ == TokenType.EOF else token.lexeme
        print(f'lox: line {token.line}, at {where}: {msg}')
        Lox.had_error = True

    @staticmethod
    def runtime_error(error):
        print(f'lox: line {error.token.line}: {error}')
        Lox.had_runtime_error = True

    @staticmethod
    def run(src):
        tokens = Lexer(src).lex()
        stmts = Parser(tokens).parse()
        if Lox.had_error:
            return
        Lox.interpreter.interpret(stmts)

    @staticmethod
    def main():
        if len(sys.argv) == 1:
            try:
                while True:
                    Lox.run(input('> '))
                    Lox.had_error = False
            except (EOFError, KeyboardInterrupt):
                print('')
                return
        else:
            for arg in sys.argv[1:]:
                with open(arg, encoding='utf8') as fp:
                    Lox.run(fp.read())
                    if Lox.had_error or Lox.had_runtime_error:
                        sys.exit(1)

if __name__ == '__main__':
    Lox.main()
