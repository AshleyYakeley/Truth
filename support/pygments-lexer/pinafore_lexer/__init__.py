import re

from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import *

__all__ = ['PinaforeLexer']

class PinaforeLexer(RegexLexer):
    name = 'Pinafore'
    filenames = ['*.pinafore']
    aliases = ['pinafore']
    mimetypes = ['text/x-pinaforesrc']

    flags = re.MULTILINE | re.UNICODE

    tokens = {
        'root': [
            (r'\n', Text),
            (r'\s+', Text),
            (r'\#(.*?)\n', Comment.Single),
            (r'{\#', Comment.Multiline, 'comment'),
            (r'"(\\\\|\\"|[^"])*"', String),
            (words(('rec', 'if', 'then', 'else', 'let', 'import', 'expose', 'in', 'do', 'case', 'of', 'end'), suffix=r'\b'), Keyword),
            (words(('datatype', 'opentype', 'subtype', 'closedtype', 'dynamictype'), suffix=r'\b'), Keyword.Declaration),
            (words(('property', 'openEntity', 'newOpenEntity', 'evaluate'), suffix=r'\b'), Keyword.Pseudo),
            (words((
                'Any', 'None', 'Literal', 'Text', 'Number', 'Rational', 'Integer', 'Boolean', 'Ordering', 'Time', 'Duration', 'Date', 'TimeOfDay', 'LocalTime',
                'Entity', 'DynamicEntity',
                'Maybe', 'Either', 'RefOrder', 'Action', 'WholeRef', 'SetRef', 'FiniteSetRef', 'ListRef', 'Element', 'Window', 'MenuItem'
                ), suffix=r'\b'), Keyword.Type),
            (r'![-0-9A-Fa-f]+', Literal.Anchor),
            (r'!"(\\\\|\\"|[^"])*"', Literal.Anchor),
            (r'~?-?[0-9][0-9.e_]*', Number),
            (r'[^\W\d]\w*', Name.Other),
            (r'(=|;|\(|\)|\[|\]|\@|::|->|,|{|}|\\)', Punctuation),
            (r'[!@$%^&*-_+\\/|\.<>?]+', Operator),
        ],
        'comment': [
            (r'[^#{]', Comment.Multiline),
            (r'{\#', Comment.Multiline, '#push'),
            (r'\#}', Comment.Multiline, '#pop'),
            (r'[#{]', Comment.Multiline)
        ]
    }
