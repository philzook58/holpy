"""Parser for expressions and rules."""

from lark import Lark, Transformer, v_args, exceptions

from geometry.expr import Fact, Rule, Line

grammar = r"""
    
    ?fact: CNAME "(" CNAME ("," CNAME)* ")"
    ?rule: fact ":-" fact ("," fact)*
    ?line: "line" "(" CNAME ("," CNAME)* ")"
    
    %import common.DIGIT
    %import common.WS
    %import common.LETTER
    %import common.CNAME
    %ignore WS
"""

@v_args(inline=True)
class GeometryTransformer(Transformer):
    def __init__(self):
        pass

    def fact(self, pred_name, *args):
        pred_name = str(pred_name)
        args = list(str(arg) for arg in args)
        return Fact(pred_name, args)

    def rule(self, concl, *assums):
        return Rule(list(assums), concl)

    def line(self, *args):
        args = list(str(arg) for arg in args)
        return Line(list(args))


fact_parser = Lark(grammar, start="fact", parser="lalr", transformer=GeometryTransformer())
rule_parser = Lark(grammar, start="rule", parser="lalr", transformer=GeometryTransformer())
line_parser = Lark(grammar, start="line", parser="lalr", transformer=GeometryTransformer())

def parse_fact(s):
    try:
        return fact_parser.parse(s)
    except TypeError as e:
        print("When parsing:", s)
        raise e

def parse_rule(s):
    return rule_parser.parse(s)

def parse_line(s):
    return line_parser.parse(s)