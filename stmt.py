
from abc import ABC, abstractmethod

class Stmt(ABC):
    @abstractmethod
    def accept(self, visitor):
        pass

class StmtVisitor(ABC):

    @abstractmethod
    def visit_block_stmt(self, stmt):
        pass

    @abstractmethod
    def visit_expression_stmt(self, stmt):
        pass

    @abstractmethod
    def visit_function_stmt(self, stmt):
        pass

    @abstractmethod
    def visit_if_stmt(self, stmt):
        pass

    @abstractmethod
    def visit_return_stmt(self, stmt):
        pass

    @abstractmethod
    def visit_var_stmt(self, stmt):
        pass

    @abstractmethod
    def visit_while_stmt(self, stmt):
        pass

    @abstractmethod
    def visit_break_stmt(self, stmt):
        pass

    @abstractmethod
    def visit_continue_stmt(self, stmt):
        pass

class Block(Stmt):
    def __init__(self, statements):
        self.statements = statements

    def accept(self, visitor):
        return visitor.visit_block_stmt(self)

class Expression(Stmt):
    def __init__(self, expression):
        self.expression = expression

    def accept(self, visitor):
        return visitor.visit_expression_stmt(self)

class Function(Stmt):
    def __init__(self, name, params, body):
        self.name = name
        self.params = params
        self.body = body

    def accept(self, visitor):
        return visitor.visit_function_stmt(self)

class If(Stmt):
    def __init__(self, condition, then_branch, else_branch):
        self.condition = condition
        self.then_branch = then_branch
        self.else_branch = else_branch

    def accept(self, visitor):
        return visitor.visit_if_stmt(self)

class Return(Stmt):
    def __init__(self, keyword, value):
        self.keyword = keyword
        self.value = value

    def accept(self, visitor):
        return visitor.visit_return_stmt(self)

class Var(Stmt):
    def __init__(self, name, initializer):
        self.name = name
        self.initializer = initializer

    def accept(self, visitor):
        return visitor.visit_var_stmt(self)

class While(Stmt):
    def __init__(self, condition, body):
        self.condition = condition
        self.body = body

    def accept(self, visitor):
        return visitor.visit_while_stmt(self)

class Break(Stmt):
    def __init__(self, keyword):
        self.keyword = keyword

    def accept(self, visitor):
        return visitor.visit_break_stmt(self)

class Continue(Stmt):
    def __init__(self, keyword):
        self.keyword = keyword

    def accept(self, visitor):
        return visitor.visit_continue_stmt(self)
