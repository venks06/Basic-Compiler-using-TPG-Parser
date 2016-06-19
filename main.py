# Name: Venkateswara Prasad Pandeti
# Id: 110396994
import sys
import tpg

variable_table = {}
functions = {}
function_variables = {}


class SemanticError(Exception):
    """
    This is the class of the exception that is raised when a semantic error
    occurs.
    """


class Stack:
    def __init__(self):
        self.items = []

    def peek(self):
        return self.items[0]

    def push(self, item):
        self.items.insert(0, item)

    def pop(self):
        return self.items.pop(0)

    def size(self):
        return len(self.items)

    def isEmpty(self):
        return self.items == []

stack = Stack()
returnStack = Stack()


class Node(object):
    def evaluate(self):
        raise Exception("Not implemented.")

    def location(self):
        raise Exception("Not implemented.")


class IntLiteral(Node):
    def __init__(self, value):
        if '.' in value:
            self.value = float(value)
        else:
            self.value = int(value)

    def evaluate(self):
        return self.value


class FloatLiteral(Node):
    def __init__(self, value):
        self.value = float(value)

    def evaluate(self):
        return self.value


class StringLiteral(Node):
    def __init__(self, value):
        self.value = str(value)
        self.value = self.value[1:len(self.value) - 1]

    def evaluate(self):
        return self.value


class Add(Node):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if not (((isinstance(left, float) or isinstance(left, int)) and
                     (isinstance(right, float) or isinstance(right, int))) or
                    (isinstance(left, str) and isinstance(right, str))):
            raise SemanticError("Invalid operation: Operands should be of int or float or string type")
        return left + right


class Subtract(Node):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if not ((isinstance(left, float) or isinstance(left, int)) and
                    (isinstance(right, float) or isinstance(right, int))):
            raise SemanticError("Invalid operation: Operands should be of int or float type")
        return left - right


class Mod(Node):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if not ((isinstance(left, float) or isinstance(left, int)) and
                    (isinstance(right, float) or isinstance(right, int))):
            raise SemanticError("Invalid operation: Operands should be of int or float type")
        return left % right


class Multiply(Node):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if right is None:
            right = 1
        if not ((isinstance(left, float) or isinstance(left, int)) and
                    (isinstance(right, float) or isinstance(right, int))):
            raise SemanticError("Invalid operation: Operands should be of int or float type")
        return left * right


class Divide(Node):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if not ((isinstance(left, float) or isinstance(left, int)) and
                    (isinstance(right, float) or isinstance(right, int))):
            raise SemanticError("Invalid operation: Operands should be of int or float type")
        if right == 0:
            raise SemanticError("Divide by 0 is undefined")
        return left / right


class Power(Node):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if not ((isinstance(left, float) or isinstance(left, int)) and
                    (isinstance(right, float) or isinstance(right, int))):
            raise SemanticError("Invalid operation: Operands should be of int or float typd")
        return left ** right


class And(Node):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def evaluate(self):
        right = self.right.evaluate()
        left = self.left.evaluate()
        if not isinstance(left, int) or not isinstance(right, int):
            raise SemanticError("Invalid operation: Operands are not integers")
        if right == 0:
            return 0
        elif left == 0:
            return 0
        else:
            return 1


class Or(Node):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if not (isinstance(left, int) and isinstance(right, int)):
            raise SemanticError("Invalid operation: Operands are not integers")
        if left == 1:
            return 1
        elif right == 1:
            return 1
        else:
            return 0


class Not(Node):
    def __init__(self, term):
        self.term = term

    def evaluate(self):
        term = self.term.evaluate()
        if not isinstance(term, int):
            raise SemanticError("Invalid operation: operand is not an integer")
        if term == 1:
            return 0
        else:
            return 1


class Xor(Node):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if not (isinstance(left, int) and isinstance(right, int)):
            raise SemanticError("Invalid operation: operands are not integers")
        return left ^ right


class Equal(Node):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if not (isinstance(left, int) or isinstance(left, float)) and \
                not (isinstance(right, float) or isinstance(right, float)):
            raise SemanticError("Invalid operation: operands are not numbers")
        if left == right:
            return 1
        else:
            return 0


class NotEqual(Node):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if not (isinstance(left, int) or isinstance(left, float)) and \
                not (isinstance(right, float) or isinstance(right, float)):
            raise SemanticError("Invalid operation: operands are not numbers")
        if left != right:
            return 1
        else:
            return 0


class Greater(Node):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if not (isinstance(left, int) or isinstance(left, float)) and \
                not (isinstance(right, float) or isinstance(right, float)):
            raise SemanticError("Invalid operation: operands are not numbers")
        if left > right:
            return 1
        else:
            return 0


class Less(Node):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def evaluate(self):
        left = self.left.evaluate()
        right = self.right.evaluate()
        if not (isinstance(left, int) or isinstance(left, float)) and \
                not (isinstance(right, float) or isinstance(right, float)):
            raise SemanticError("Invalid operation: operands are not numbers")
        if left < right:
            return 1
        else:
            return 0


class ListIndex(Node):
    def __init__(self, lst, index):
        self.lst = lst
        self.index = index

    def evaluate(self):
        lst = self.lst.evaluate()
        index = self.index.evaluate()
        if not isinstance(index, int):
            raise SemanticError("Invalid operation: lists indexes are not integers")
        length = len(lst)
        if index > length - 1:
            raise SemanticError("Invalid operation: Index out of bounds")
        return lst[index]


class EmptyList(Node):
    def evaluate(self):
        return []

    def execute(self):
        return self.evaluate()


class StringIndex(Node):
    def __init__(self, string, index):
        self.string = string
        self.index = index

    def evaluate(self):
        string = self.string.evaluate()
        index = self.index.evaluate()
        if not isinstance(index, int):
            raise SemanticError("Invalid operation: strings indexes are not integers")
        length = len(string)
        if index > length - 1:
            raise SemanticError("Invalid operation: Index out of bounds")
        return string[index]


class PrepareList(Node):
    def __init__(self, element1, element2):
        self.element1 = element1
        self.element2 = element2

    def evaluate(self):
        lst = []
        if self.element1 is not None:
            element1 = self.element1.evaluate()
        if self.element2 is not None:
            element2 = self.element2.evaluate()
        size1 = 0
        size2 = 0
        if not isinstance(element1, list):
            size1 = 0
        else:
            size1 = get_size(element1)
        if not isinstance(element2, list):
            size2 = 0
        else:
            size2 = get_size(element2)

        if size1 > size2:
            lst.extend(element1)
        else:
            lst.append(element1)
        lst.append(element2)
        return lst


def get_size(lst):
    size = 1
    for element in lst:
        if isinstance(element, list):
            size += 1;
        else:
            break
    return size;


class OneElementInList(Node):
    def __init__(self, element):
        self.element = element

    def evaluate(self):
        lst = []
        if self.element is not None:
            element = self.element.evaluate()
            lst.append(element)
        return lst


class Bracket(Node):
    def __init__(self, statements):
        self.statements = statements

    def evaluate(self):
        for i in self.statements:
            i.evaluate()


class Print:
    def __init__(self, expression):
        self.expression = expression

    def evaluate(self):
        value = self.expression.evaluate();
        print(value)


class Variable:
    def __init__(self, variable):
        self.variable = str(variable)

    def evaluate(self):
        global variable_table, stack
        item = stack.peek();
        i = 0;
        if self.variable in variable_table:
            return variable_table[self.variable]
        for arg in item.arguments:
            if arg.location() == self.variable:
                return item.values[i].evaluate()
            i = i + 1
        raise SemanticError("Invalid operation: Variable undefined")

    def location(self):
        return self.variable


class VariableIndex:
    def __init__(self, variable, index):
        self.index = index
        self.variable = variable

    def evaluate(self):
        index = self.index.evaluate()
        variable = self.variable.evaluate()
        if not isinstance(index, int):
            raise SemanticError("Invalid operation: Indexes are not integers")
        if not isinstance(variable, list):
            raise SemanticError("Invalid operation: Should be of type list")
        if index > len(variable) - 1:
            raise SemanticError("Invalid operation: Index out of bounds")
        return variable[index]

    def location(self):
        return self.variable.location(), self.index.evaluate()


class While(Node):
    def __init__(self, conditionexpr, statement):
        self.conditionexpr = conditionexpr
        self.statement = statement

    def evaluate(self):
        while self.conditionexpr.evaluate() == 1:
            self.statement.evaluate()


class Assign:
    def __init__(self, variable, value):
        self.variable = variable
        self.value = value

    def evaluate(self):
        global variable_table
        # variable_table[self.variable] = self.value
        if isinstance(self.variable, VariableIndex):
            variable1, index = self.variable.location()
            variable2 = variable_table[variable1]
            variable2[index] = self.value.evaluate()
        else:
            variable_table[self.variable.location()] = self.value.evaluate()


class IfElse(Node):
    def __init__(self, conditionexpr, ifblock, elseblock):
        self.conditionexpr = conditionexpr
        self.ifblock = ifblock
        self.elseblock = elseblock

    def evaluate(self):
        # if condition expression is evaluated to true (1)
        # expressions = []
        if self.conditionexpr.evaluate() == 1:
            self.ifblock.evaluate()
        else:
            self.elseblock.evaluate()


class If(Node):
    def __init__(self, conditionexpr, ifblock):
        self.conditionexpr = conditionexpr
        self.ifblock = ifblock

    def evaluate(self):
        if self.conditionexpr.evaluate() == 1:
            self.ifblock.evaluate()


class FunctionObject:
    def __init__(self, name, arguments, block):
        self.name = name
        self.arguments = arguments
        self.block = block


class Return(Node):
    def __init__(self, value):
        self.value = value

    def evaluate(self):
        value = self.value.evaluate()
        returnStack.push(value)


class ProcDef(Node):
    def __init__(self, method, arguments, block):
        self.method = method
        self.arguments = arguments
        self.block = block
        obj = FunctionObject(method, arguments, block)
        functions[self.method.location()] = obj
        function_variables[method.location()] = arguments


class StackItem:
    def __init__(self, method, arguments, values):
        self.method = method
        self.arguments = arguments
        self.values = values


class ProcedureCall(Node):
    def __init__(self, method, arguments):
        self.method = method
        self.arguments = arguments

    def evaluate(self):
        method = self.method
        if (method.location() in functions):
            functor = functions[method.location()]
            if stack.isEmpty():
                obj = StackItem(method.location, functor.arguments, self.arguments)
            else:
                lst = []
                for arg in self.arguments:
                    value = arg.evaluate()
                    lst.append(IntLiteral(str(value)))
                obj = StackItem(method.location, functor.arguments, lst)
            stack.push(obj)
            functor.block.evaluate()
            stack.pop()
            return returnStack.pop()


def make_op(op):
    return {
        '+': lambda x, y: x + y,
        '-': lambda x, y: x - y,
        '*': lambda x, y: x * y,
        '/': lambda x, y: x / y,
        '%': lambda x, y: x % y,
    }[op]


# This is the TPG Parser that is responsible for turning our language into
# an abstract syntax tree.
class Parser(tpg.Parser):
    """
    token int '(\d+\.\d*|\d*\.\d+)|(\d+)' IntLiteral;
    token string '\"(\\.|[^\\"])*\"' StringLiteral;
    token variable '[A-Za-z][A-Za-z0-9]*' Variable;
    separator space "\s+";

    START/a -> (statement/a)* ;

    statement/a -> ( _func_def/a | block/a | code/a );

    block/a -> "\{" $ a = Bracket([]) $ ( statement/b $ a.statements.append(b) $ )* "\}";

    code/a -> ( _if_else/a | _if/a | _while/a | lines/a );
    lines/a -> ( _assign/a | _print/a | _return/a | _function/a ) ";" ;

    _func_def/a -> variable/v params/n block/b    $ a = ProcDef(v, n, b) $ ;
    _if_else/a -> "if" "\(" expression/e "\)" statement/d "else" statement/s   $ a = IfElse(e,d,s) $;
    _if/a -> "if" "\(" expression/e "\)" statement/s       $ a = If(e, s) $;
    _while/a -> "while" "\(" expression/e "\)" statement/s $ a = While(e, s) $;
    _assign/a -> expression/a "=(?!=)" expression/b        $ a = Assign(a, b) $ ;
    _print/a -> "print" expression/a                       $ a = Print(a) $ ;
    _return/a -> "return " expression/a                    $ a = Return(a) $ ;
    _function/a -> variable/v param_list/l                 $ a = ProcedureCall(v, l) $ ;

    expression/a -> OR/a;
    OR/a -> AND/a ( "or" AND/b $ a = Or(a, b) $ )* ;
    AND/a -> NOT/a ( "and" NOT/b $ a = And(a, b) $ )* ;
    NOT/a -> "not" NOT/a $ a = Not(a) $ | compare/a ;

    compare/a -> xor/a
    ( "\<" xor/b $ a = Less(a, b) $
    | "\>" xor/b $ a = Greater(a, b) $
    | "\=\=" xor/b $ a = Equal(a, b) $
    )* ;

    xor/a -> mod/a ( "xor" mod/b $ a = Xor(a, b) $ )* ;
               mod/a -> addsub/a ( "\%" addsub/b $ a = Mod(a,b) $)*;

    addsub/a -> muldiv/a
    ( "\+" muldiv/b $ a = Add(a, b) $
    | "\-"  muldiv/b $ a = Subtract(a, b) $
    )* ;

    muldiv/a -> index/a
    ( "\*" index/b $ a = Multiply(a, b) $
    | "/"  index/b $ a = Divide(a, b) $
    )* ;

    index/a -> parens/a ( "\[" expression/b "\]" $ a = VariableIndex(a, b) $ )*;

    parens/a -> _function/a | "\(" expression/a "\)" | literal/a | variable/a;

    literal/a ->
    ( int/a
    | string/a
    | "\[" expression/b $ a = ListLiteral(b) $
      ( "," expression/b $ a.elements.append(b) $ )*
      "\]"
      );

    params/a -> "\(" $ a = [] $ ( variable/v $ a.append(v) $ )?
    ( "," variable/v $ a.append(v) $ )*
    "\)"
    ;

    param_list/a -> "\(" $ a = [] $ ( expression/e $ a.append(e) $ )?
    ( "," expression/e $ a.append(e) $ )*
    "\)";

   """


# Make an instance of the parser. This acts like a function.
parse = Parser()

# This is the driver code, that reads in lines, deals with errors, and
# prints the output if no error occurs.

# Open the file containing the input.
try:
    f = open(sys.argv[1], "r")
except(IndexError, IOError):
    f = open("input1.txt", "r")
try:
    input = f.read()
    # parse block wise
    node = parse(input)
    node.evaluate()
# If an exception is thrown, print the appropriate error.
except Exception, e:
    print str(e)
f.close()
