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
            (r'"(\\\\|\\"|[^"])*"', String),
            (r'(opentype|subtype|closedtype|property|entity)\b', Keyword.Declaration),
            (words(('if', 'then', 'else', 'let', 'in', 'do', 'case', 'of', 'end'), suffix=r'\b'), Keyword),
            (words((
                'Any', 'None', 'Literal', 'Text', 'Number', 'Rational', 'Integer', 'Boolean', 'Entity', 'NewEntity',
                'Maybe', 'Either', 'Order', 'Action', 'Ref', 'FiniteSetRef', 'UI', 'Window', 'MenuItem'
                ), suffix=r'\b'), Keyword.Type),
            (r'![-0-9A-Fa-f]+', Literal.Anchor),
            (r'!"(\\\\|\\"|[^"])*"', Literal.Anchor),
            (r'~?-?[0-9][0-9.e_]*', Number),
            (r'[^\W\d]\w*', Name.Other),
            (r'(=|;|\(|\)|\[|\]|\@|::|->|,|{|}|\\)', Punctuation),
            (r'[!@$%^&*-_+\\/|\.<>?]+', Operator),
        ]
    }
