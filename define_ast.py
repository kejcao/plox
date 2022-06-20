def define_ast(ast_name, types):
    with open(f'{ast_name.lower()}.py', 'w', encoding='utf8') as fp:
        fp.write(
f'''
from abc import ABC, abstractmethod

class {ast_name}(ABC):
    @abstractmethod
    def accept(self, visitor):
        pass

class {ast_name}Visitor(ABC):
'''
        )

        for name in types:
            fp.write(
f'''
    @abstractmethod
    def visit_{name.lower()}_{ast_name.lower()}(self, {ast_name.lower()}):
        pass
'''
            )

        for name, fields in types.items():
            fp.write(
f'''
class {name}({ast_name}):
    def __init__(self, {", ".join(fields)}):
'''
            )

            for f in fields:
                fp.write(f'        self.{f} = {f}\n')

            fp.write(
f'''
    def accept(self, visitor):
        return visitor.visit_{name.lower()}_{ast_name.lower()}(self)
'''
            )

define_ast('Expr', {
    'Assign': ['name', 'value'],
    'Binary': ['left', 'operator', 'right'],
    'Call': ['callee', 'paren', 'arguments'],
    'Get': ['object_', 'name'],
    'Grouping': ['expression'],
    'Literal': ['value'],
    'Logical': ['left', 'operator', 'right'],
    'Set': ['object_', 'name', 'value'],
    'Super': ['keyword', 'method'],
    'This': ['keyword'],
    'Unary': ['operator', 'right'],
    'Variable': ['name']
})

define_ast('Stmt', {
    'Block': ['statements'],
    'Expression': ['expression'],
    'If': ['condition', 'then_branch', 'else_branch'],
    'Print': ['expression'],
    'Var': ['name', 'initializer'],
    'While': ['condition', 'body'],
    'Break': ['keyword']
})
