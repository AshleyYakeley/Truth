import re

from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import *
import importlib.resources, json

__all__ = ['PinaforeLexer']

with open(importlib.resources.files('pinafore_lexer').joinpath('syntax-data.json')) as f:
    syntaxData = json.load(f)

def getTokenType(t):
    match t:
        case "keyword.control.pinafore":
            return Keyword
        case "keyword.declaration.pinafore":
            return Keyword.Declaration
        case "keyword.other.pinafore":
            return Keyword
        case _:
            return t # will give error, add new entry

keywordMatchers = [(words(names, suffix=r'\b'), getTokenType(t)) for t,names in syntaxData["keywords"].items()]

typeMatcher = (words(syntaxData["types"], suffix=r'\b'), Keyword.Type)

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
        ] + keywordMatchers + [
            typeMatcher,
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
            (r'[#{]', Comment.Multiline),
        ]
    }
