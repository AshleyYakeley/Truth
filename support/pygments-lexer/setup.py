#!/usr/bin/env python3
"""Setup pymdown-lexers."""
from setuptools import setup, find_packages

entry_points = '''
[pygments.lexers]
pinafore=pinafore_lexer:PinaforeLexer
'''

setup(
    name='pinafore-lexer',
    version='0.1.0',
    description='Pygments lexer for Pinafore',
    author='Ashley Yakeley',
    author_email='ashley@semantic.org',
    url='https://github.com/AshleyYakeley/Truth',
    packages=find_packages(),
    entry_points=entry_points,
    install_requires=[
        'Pygments>=2.0.1'
    ],
    zip_safe=True,
    license='GPL-2'
)
