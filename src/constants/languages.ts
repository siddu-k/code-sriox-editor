import { ProgrammingLanguage, EditorTheme } from '@/types/compiler';
import { themes } from 'prism-react-renderer';

// Define supported languages
export const LANGUAGES: ProgrammingLanguage[] = [
  {
    id: 71,
    name: 'Python',
    defaultCode: 'print("Hello, World!")',
    prismLang: 'python',
    logo: '🐍'
  },
  {
    id: 63,
    name: 'JavaScript',
    defaultCode: 'console.log("Hello, World!");',
    prismLang: 'javascript',
    logo: '📜'
  },
  {
    id: 54,
    name: 'C++',
    defaultCode: '#include <iostream>\n\nint main() {\n    std::cout << "Hello, World!" << std::endl;\n    return 0;\n}',
    prismLang: 'cpp',
    logo: '⚙️'
  },
  {
    id: 50,
    name: 'C',
    defaultCode: '#include <stdio.h>\n\nint main() {\n    printf("Hello, World!\\n");\n    return 0;\n}',
    prismLang: 'c',
    logo: '🧰'
  },
  {
    id: 62,
    name: 'Java',
    defaultCode: 'public class Main {\n    public static void main(String[] args) {\n        System.out.println("Hello, World!");\n    }\n}',
    prismLang: 'java',
    logo: '☕'
  },
  {
    id: 73,
    name: 'Rust',
    defaultCode: 'fn main() {\n    println!("Hello, World!");\n}',
    prismLang: 'rust',
    logo: '🦀'
  },
  {
    id: 84,
    name: 'Kotlin',
    defaultCode: 'fun main() {\n    println("Hello, World!")\n}',
    prismLang: 'kotlin',
    logo: '🤖'
  },
  {
    id: 40,
    name: 'C#',
    defaultCode: 'using System;\n\npublic class Program {\n    public static void Main(string[] args) {\n        Console.WriteLine("Hello, World!");\n    }\n}',
    prismLang: 'csharp',
    logo: '💠'
  },
  {
    id: 70,
    name: 'PHP',
    defaultCode: `<?php\n\necho "Hello, World!\\n";\n\n?>`,
    prismLang: 'php',
    logo: '🐘'
  },
  {
    id: 43,
    name: 'Objective-C',
    defaultCode: `#import <Foundation/Foundation.h>\n\nint main(int argc, const char * argv[]) {\n    @autoreleasepool {\n        NSLog(@"Hello, World!");\n    }\n    return 0;\n}`,
    prismLang: 'objectivec',
    logo: '🍎'
  },
  {
    id: 74,
    name: 'Swift',
    defaultCode: 'import Foundation\n\nprint("Hello, World!")',
    prismLang: 'swift',
    logo: '🐦'
  },
  {
    id: 51,
    name: 'Go',
    defaultCode: 'package main\n\nimport "fmt"\n\nfunc main() {\n    fmt.Println("Hello, World!")\n}',
    prismLang: 'go',
    logo: '🐹'
  },
  {
    id: 60,
    name: 'Ruby',
    defaultCode: 'puts "Hello, World!"',
    prismLang: 'ruby',
    logo: '💎'
  },
  {
    id: 65,
    name: 'TypeScript',
    defaultCode: 'console.log("Hello, World!");',
    prismLang: 'typescript',
    logo: '📘'
  },
  {
    id: 46,
    name: 'Dart',
    defaultCode: 'void main() {\n  print("Hello, World!");\n}',
    prismLang: 'dart',
    logo: '🎯'
  },
  {
    id: 68,
    name: 'R',
    defaultCode: 'print("Hello, World!")',
    prismLang: 'r',
    logo: '📊'
  },
  {
    id: 61,
    name: 'Scala',
    defaultCode: 'object Main {\n  def main(args: Array[String]): Unit = {\n    println("Hello, World!")\n  }\n}',
    prismLang: 'scala',
    logo: '⚙️'
  },
  {
    id: 56,
    name: 'Groovy',
    defaultCode: 'println "Hello, World!"',
    prismLang: 'groovy',
    logo: '🍇'
  },
  {
    id: 47,
    name: 'Erlang',
    defaultCode: 'io:format("Hello, World!~n", []).',
    prismLang: 'erlang',
    logo: '💡'
  },
  {
    id: 55,
    name: 'Haskell',
    defaultCode: 'main = putStrLn "Hello, World!"',
    prismLang: 'haskell',
    logo: 'λ'
  },
  {
    id: 17,
    name: 'Pascal',
    defaultCode: `program HelloWorld;\nbegin\n  writeln('Hello, World!');\nend.`,
    prismLang: 'pascal',
    logo: '📝'
  },
  {
    id: 86,
    name: 'Lua',
    defaultCode: 'print("Hello, World!")',
    prismLang: 'lua',
    logo: '🌙'
  },
  {
    id: 78,
    name: 'Scheme',
    defaultCode: '(display "Hello, World!")\n(newline)',
    prismLang: 'scheme',
    logo: '📜'
  },
  {
    id: 77,
    name: 'Fortran',
    defaultCode: `program hello\n  print *, "Hello, World!"\nend program hello`,
    prismLang: 'fortran',
    logo: '🧮'
  },
  {
    id: 80,
    name: 'SQL',
    defaultCode: 'SELECT "Hello, World!";',
    prismLang: 'sql',
    logo: '🗄️'
  }
];

// Define editor themes
export const BLACK_AND_WHITE_THEME = {
  plain: {
    color: '#ffffff',
    backgroundColor: '#000000',
  },
  styles: [
    {
      types: ['comment', 'prolog', 'doctype', 'cdata'],
      style: {
        color: '#999999',
        fontStyle: 'italic',
      },
    },
    {
      types: ['namespace'],
      style: {
        opacity: 0.7,
      },
    },
    {
      types: ['string', 'attr-value'],
      style: {
        color: '#ffffff',
      },
    },
    {
      types: ['punctuation', 'operator'],
      style: {
        color: '#cccccc',
      },
    },
    {
      types: ['entity', 'url', 'symbol', 'number', 'boolean', 'variable', 'constant', 'property', 'regex', 'inserted'],
      style: {
        color: '#dddddd',
      },
    },
    {
      types: ['atrule', 'keyword', 'attr-name', 'selector'],
      style: {
        color: '#ffffff',
        fontWeight: 'bold',
      },
    },
    {
      types: ['function', 'deleted', 'tag'],
      style: {
        color: '#ffffff',
      },
    },
    {
      types: ['function-variable'],
      style: {
        color: '#eeeeee',
      },
    },
    {
      types: ['tag', 'selector', 'keyword'],
      style: {
        color: '#ffffff',
        fontWeight: 'bold',
      },
    },
  ],
};

export const EDITOR_THEMES: EditorTheme[] = [
  {
    name: 'Night Owl',
    value: 'nightOwl',
    theme: themes.nightOwl
  },
  {
    name: 'Dracula',
    value: 'dracula',
    theme: themes.dracula
  },
  {
    name: 'VS Dark',
    value: 'vsDark',
    theme: themes.vsDark
  },
  {
    name: 'Duotone',
    value: 'duotoneDark',
    theme: themes.duotoneDark
  },
  {
    name: 'Oceanic',
    value: 'oceanicNext',
    theme: themes.oceanicNext
  },
  {
    name: 'Black & White',
    value: 'blackAndWhite',
    theme: BLACK_AND_WHITE_THEME
  }
];

export const DEFAULT_UI_SIZE = 100;
export const MIN_UI_SIZE = 60;
export const MAX_UI_SIZE = 150;
