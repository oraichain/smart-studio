// Copyright (c) 2012-2018, Matt Godbolt
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
//     * Redistributions of source code must retain the above copyright notice,
//       this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

// Derrived and modified from
// https://github.com/mattgodbolt/compiler-explorer/blob/0a87dcb00abfc5931067a0eaf961b68a1d0a9bac/static/rust-mode.js

type IRichLanguageConfiguration = monaco.languages.LanguageConfiguration;
type ILanguage = monaco.languages.IMonarchLanguage;
type IModel = monaco.editor.IModel;
type IPosition = monaco.IPosition;

let completionItems: monaco.languages.CompletionItem[] = null;
function getCompletionItems(): monaco.languages.CompletionItem[] {
  const keyword = monaco.languages.CompletionItemKind.Keyword;
  if (completionItems) {
    return completionItems;
  }
  return (completionItems = []);
}

const LanguageConfiguration: IRichLanguageConfiguration = {
  comments: {
    lineComment: '//',
    blockComment: ['/*', '*/']
  },
  brackets: [['{', '}'], ['[', ']'], ['(', ')']],
  autoClosingPairs: [
    { open: '[', close: ']' },
    { open: '{', close: '}' },
    { open: '(', close: ')' },
    { open: "'", close: "'", notIn: ['string', 'comment'] },
    { open: '"', close: '"', notIn: ['string'] }
  ],
  surroundingPairs: [{ open: '{', close: '}' }, { open: '[', close: ']' }, { open: '(', close: ')' }, { open: '"', close: '"' }, { open: "'", close: "'" }],
  folding: {
    markers: {
      start: new RegExp('^\\s*#pragma\\s+region\\b'),
      end: new RegExp('^\\s*#pragma\\s+endregion\\b')
    }
  }
};

const MonarchDefinitions = <monaco.languages.IMonarchLanguage>{
  defaultToken: 'invalid',
  tokenPostfix: '.rust',
  keywords: [
    'as',
    'async',
    'await',
    'box',
    'break',
    'const',
    'continue',
    'crate',
    'dyn',
    'else',
    'enum',
    'extern',
    'false',
    'fn',
    'for',
    'if',
    'impl',
    'in',
    'let',
    'loop',
    'match',
    'mod',
    'move',
    'mut',
    'pub',
    'ref',
    'return',
    'self',
    'static',
    'struct',
    'super',
    'trait',
    'true',
    'try',
    'type',
    'unsafe',
    'use',
    'where',
    'while',
    'catch',
    'default',
    'union',
    'static',
    'abstract',
    'alignof',
    'become',
    'do',
    'final',
    'macro',
    'offsetof',
    'override',
    'priv',
    'proc',
    'pure',
    'sizeof',
    'typeof',
    'unsized',
    'virtual',
    'yield'
  ],

  typeKeywords: [
    'Self',
    'm32',
    'm64',
    'm128',
    'f80',
    'f16',
    'f128',
    'int',
    'uint',
    'float',
    'char',
    'bool',
    'u8',
    'u16',
    'u32',
    'u64',
    'f32',
    'f64',
    'i8',
    'i16',
    'i32',
    'i64',
    'str',
    'Option',
    'Either',
    'c_float',
    'c_double',
    'c_void',
    'FILE',
    'fpos_t',
    'DIR',
    'dirent',
    'c_char',
    'c_schar',
    'c_uchar',
    'c_short',
    'c_ushort',
    'c_int',
    'c_uint',
    'c_long',
    'c_ulong',
    'size_t',
    'ptrdiff_t',
    'clock_t',
    'time_t',
    'c_longlong',
    'c_ulonglong',
    'intptr_t',
    'uintptr_t',
    'off_t',
    'dev_t',
    'ino_t',
    'pid_t',
    'mode_t',
    'ssize_t'
  ],

  constants: ['true', 'false', 'Some', 'None', 'Left', 'Right', 'Ok', 'Err'],
  supportConstants: [
    'EXIT_FAILURE',
    'EXIT_SUCCESS',
    'RAND_MAX',
    'EOF',
    'SEEK_SET',
    'SEEK_CUR',
    'SEEK_END',
    '_IOFBF',
    '_IONBF',
    '_IOLBF',
    'BUFSIZ',
    'FOPEN_MAX',
    'FILENAME_MAX',
    'L_tmpnam',
    'TMP_MAX',
    'O_RDONLY',
    'O_WRONLY',
    'O_RDWR',
    'O_APPEND',
    'O_CREAT',
    'O_EXCL',
    'O_TRUNC',
    'S_IFIFO',
    'S_IFCHR',
    'S_IFBLK',
    'S_IFDIR',
    'S_IFREG',
    'S_IFMT',
    'S_IEXEC',
    'S_IWRITE',
    'S_IREAD',
    'S_IRWXU',
    'S_IXUSR',
    'S_IWUSR',
    'S_IRUSR',
    'F_OK',
    'R_OK',
    'W_OK',
    'X_OK',
    'STDIN_FILENO',
    'STDOUT_FILENO',
    'STDERR_FILENO'
  ],

  supportMacros: ['format!', 'print!', 'println!', 'panic!', 'format_args!', 'unreachable!', 'write!', 'writeln!'],
  operators: [
    '!',
    '!=',
    '%',
    '%=',
    '&',
    '&=',
    '&&',
    '*',
    '*=',
    '+',
    '+=',
    '-',
    '-=',
    '->',
    '.',
    '..',
    '...',
    '/',
    '/=',
    ':',
    ';',
    '<<',
    '<<=',
    '<',
    '<=',
    '=',
    '==',
    '=>',
    '>',
    '>=',
    '>>',
    '>>=',
    '@',
    '^',
    '^=',
    '|',
    '|=',
    '||',
    '_',
    '?',
    '#'
  ],

  symbols: /[\#\!\%\&\*\+\-\.\/\:\;\<\=\>\@\^\|_\?]+/,
  escapes: /\\([nrt0\"''\\]|x\h{2}|u\{\h{1,6}\})/,
  delimiters: /[,]/,
  intSuffixes: /[iu](8|16|32|64|128|size)/,
  floatSuffixes: /f(32|64)/,
  // The main tokenizer for our languages
  tokenizer: {
    root: [
      [
        /[a-zA-Z][a-zA-Z0-9_]*!?|_[a-zA-Z0-9_]+/,
        {
          cases: {
            '@typeKeywords': 'keyword.type',
            '@keywords': 'keyword',
            '@supportConstants': 'keyword',
            '@supportMacros': 'keyword',
            '@constants': 'keyword',
            '@default': 'identifier'
          }
        }
      ],
      // Designator
      [/\$/, 'identifier'],
      // Lifetime annotations
      [/'[a-zA-Z_][a-zA-Z0-9_]*(?=[^\'])/, 'identifier'],
      // Byte literal
      [/'\S'/, 'string.byteliteral'],
      // Strings
      [/"/, { token: 'string.quote', bracket: '@open', next: '@string' }],
      { include: '@numbers' },
      // Whitespace + comments
      { include: '@whitespace' },
      [
        /@delimiters/,
        {
          cases: {
            '@keywords': 'keyword',
            '@default': 'delimiter'
          }
        }
      ],

      [/[{}()\[\]<>]/, '@brackets'],
      [/@symbols/, { cases: { '@operators': 'operator', '@default': '' } }]
    ],

    whitespace: [[/[ \t\r\n]+/, 'white'], [/\/\*/, 'comment', '@comment'], [/\/\/.*$/, 'comment']],

    comment: [[/[^\/*]+/, 'comment'], [/\/\*/, 'comment', '@push'], ['\\*/', 'comment', '@pop'], [/[\/*]/, 'comment']],

    string: [[/[^\\"]+/, 'string'], [/@escapes/, 'string.escape'], [/\\./, 'string.escape.invalid'], [/"/, { token: 'string.quote', bracket: '@close', next: '@pop' }]],
    numbers: [
      //Octal
      [/(0o[0-7_]+)(@intSuffixes)?/, { token: 'number' }],
      //Binary
      [/(0b[0-1_]+)(@intSuffixes)?/, { token: 'number' }],
      //Exponent
      [/[\d][\d_]*(\.[\d][\d_]*)?[eE][+-][\d_]+(@floatSuffixes)?/, { token: 'number' }],
      //Float
      [/\b(\d\.?[\d_]*)(@floatSuffixes)?\b/, { token: 'number' }],
      //Hexadecimal
      [/(0x[\da-fA-F]+)_?(@intSuffixes)?/, { token: 'number' }],
      //Integer
      [/[\d][\d_]*(@intSuffixes?)?/, { token: 'number' }]
    ]
  }
};

export const Rust = {
  MonarchDefinitions,
  LanguageConfiguration,
  CompletionItemProvider: {
    provideCompletionItems: (model: IModel, position: IPosition): monaco.languages.CompletionItem[] => {
      return getCompletionItems();
    }
  },
  HoverProvider: {
    provideHover: (model: IModel, position: IPosition): any => {
      return {
        range: new monaco.Range(1, 1, model.getLineCount(), model.getLineMaxColumn(model.getLineCount())),
        contents: ['**DETAILS**', { language: 'html', value: 'TODO' }]
      };
    }
  }
};
