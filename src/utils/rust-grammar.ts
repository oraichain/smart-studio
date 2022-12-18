import { language as rustLanguage, conf as rustConf } from 'monaco-editor/esm/vs/basic-languages/rust/rust';

export const language: monaco.languages.IMonarchLanguage = {
  // Set defaultToken to invalid to see what you do not tokenize yet
  ...rustLanguage,
  controlFlowKeywords: ['continue', 'else', 'for', 'if', 'while', 'loop', 'match'],
  // The main tokenizer for our languages
  tokenizer: {
    root: [
      // identifiers and keywords
      [
        /[a-z_$][\w$]*/,
        {
          cases: {
            '@typeKeywords': 'type.identifier',
            '@keywords': {
              cases: {
                fn: { token: 'keyword', next: '@func_decl' },
                '@default': 'keyword'
              }
            },
            '@controlFlowKeywords': 'keyword.control',
            '@default': 'variable'
          }
        }
      ],
      [/[A-Z][\w\$]*/, 'type.identifier'], // to show class names nicely

      // whitespace
      { include: '@whitespace' },

      // delimiters and operators
      [/[{}()\[\]]/, '@brackets'],
      [/[<>](?!@symbols)/, '@brackets'],
      [
        /@symbols/,
        {
          cases: {
            '@operators': 'operator',
            '@default': ''
          }
        }
      ],

      // @ annotations.
      // As an example, we emit a debugging log message on these tokens.
      // Note: message are supressed during the first load -- change some lines to see them.
      [/@\s*[a-zA-Z_\$][\w\$]*/, { token: 'annotation', log: 'annotation token: $0' }],

      // numbers
      [/\d*\.\d+([eE][\-+]?\d+)?/, 'number.float'],
      [/0[xX][0-9a-fA-F]+/, 'number.hex'],
      [/\d+/, 'number'],

      // delimiter: after number because of .\d floats
      [/[;,.]/, 'delimiter'],

      // strings
      [/"([^"\\]|\\.)*$/, 'string.invalid'], // non-teminated string
      [/"/, { token: 'string.quote', bracket: '@open', next: '@string' }],

      // characters
      [/'[^\\']'/, 'string'],
      [/(')(@escapes)(')/, ['string', 'string.escape', 'string']],
      [/'/, 'string.invalid']
    ],

    comment: [
      [/[^\/*]+/, 'comment'],
      [/\/\*/, 'comment', '@push'], // nested comment
      ['\\*/', 'comment', '@pop'],
      [/[\/*]/, 'comment']
    ],

    string: [[/[^\\"]+/, 'string'], [/@escapes/, 'string.escape'], [/\\./, 'string.escape.invalid'], [/"/, { token: 'string.quote', bracket: '@close', next: '@pop' }]],

    whitespace: [[/[ \t\r\n]+/, 'white'], [/\/\*/, 'comment', '@comment'], [/\/\/.*$/, 'comment']],

    func_decl: [[/[a-z_$][\w$]*/, 'support.function', '@pop']],

    func_call: [[/[a-z_$][\w$]*(?=\s*\()/, 'support.function', '@pop'], [/.*/, '']]
  }
};

export const conf: monaco.languages.LanguageConfiguration = rustConf;
