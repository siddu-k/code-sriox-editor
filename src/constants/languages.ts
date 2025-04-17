import { ProgrammingLanguage, EditorTheme } from '@/types/compiler';
import { themes } from 'prism-react-renderer';

export const LANGUAGES: ProgrammingLanguage[] = [
  {
    id: 63,
    name: 'JavaScript',
    defaultCode: `console.log('Hello, World!');`,
    prismLang: 'javascript',
    logo: '⚛️'
  },
  {
    id: 77,
    name: 'TypeScript',
    defaultCode: `console.log('Hello, TypeScript!');`,
    prismLang: 'typescript',
    logo: '🟦'
  },
  {
    id: 54,
    name: 'Java',
    defaultCode: `public class Main {\n  public static void main(String[] args) {\n    System.out.println("Hello, Java!");\n  }\n}`,
    prismLang: 'java',
    logo: '☕'
  },
  {
    id: 62,
    name: 'Python',
    defaultCode: `print("Hello, Python!")`,
    prismLang: 'python',
    logo: '🐍'
  },
  {
    id: 71,
    name: 'C++',
    defaultCode: `#include <iostream>\n\nint main() {\n  std::cout << "Hello, C++!" << std::endl;\n  return 0;\n}`,
    prismLang: 'cpp',
    logo: '🧪'
  },
  {
    id: 51,
    name: 'C',
    defaultCode: `#include <stdio.h>\n\nint main() {\n  printf("Hello, C!\\n");\n  return 0;\n}`,
    prismLang: 'c',
    logo: '⚙️'
  },
  {
    id: 43,
    name: 'C#',
    defaultCode: `using System;\n\npublic class Program {\n  public static void Main(string[] args) {\n    Console.WriteLine("Hello, C#!");\n  }\n}`,
    prismLang: 'csharp',
    logo: '💠'
  },
  {
    id: 60,
    name: 'PHP',
    defaultCode: `<?php\necho "Hello, PHP!";\n?>`,
    prismLang: 'php',
    logo: '🐘'
  },
  {
    id: 80,
    name: 'Ruby',
    defaultCode: `puts "Hello, Ruby!"`,
    prismLang: 'ruby',
    logo: '♦️'
  },
  {
    id: 37,
    name: 'Go',
    defaultCode: `package main\n\nimport "fmt"\n\nfunc main() {\n  fmt.Println("Hello, Go!")\n}`,
    prismLang: 'go',
    logo: '🐹'
  },
  {
    id: 47,
    name: 'Kotlin',
    defaultCode: `fun main() {\n  println("Hello, Kotlin!")\n}`,
    prismLang: 'kotlin',
    logo: '🤖'
  },
  {
    id: 73,
    name: 'Swift',
    defaultCode: `print("Hello, Swift!")`,
    prismLang: 'swift',
    logo: '🍎'
  },
  {
    id: 46,
    name: 'Scala',
    defaultCode: `object Main {\n  def main(args: Array[String]): Unit = {\n    println("Hello, Scala!")\n  }\n}`,
    prismLang: 'scala',
    logo: 'Ｓ'
  },
  {
    id: 85,
    name: 'Rust',
    defaultCode: `fn main() {\n  println!("Hello, Rust!");\n}`,
    prismLang: 'rust',
    logo: '🦀'
  },
  {
    id: 22,
    name: 'Dart',
    defaultCode: `void main() {\n  print('Hello, Dart!');\n}`,
    prismLang: 'dart',
    logo: '🎯'
  },
  {
    id: 65,
    name: 'R',
    defaultCode: `print("Hello, R!")`,
    prismLang: 'r',
    logo: '📊'
  },
  {
    id: 33,
    name: 'Erlang',
    defaultCode: `-module(main).\n-export([main/0]).\n\nmain() ->\n  io:format("Hello, Erlang!~n").`,
    prismLang: 'erlang',
    logo: '💡'
  },
  {
    id: 44,
    name: 'Groovy',
    defaultCode: `println "Hello, Groovy!"`,
    prismLang: 'groovy',
    logo: '🍇'
  },
  {
    id: 55,
    name: 'Lua',
    defaultCode: `print("Hello, Lua!")`,
    prismLang: 'lua',
    logo: '🌙'
  },
  {
    id: 78,
    name: 'V',
    defaultCode: `fn main() {\n\tprintln('Hello, V!')\n}`,
    prismLang: 'v',
    logo: 'Ｖ'
  },
  {
    id: 26,
    name: 'Elixir',
    defaultCode: `IO.puts "Hello, Elixir!"`,
    prismLang: 'elixir',
    logo: '🧪'
  },
  {
    id: 30,
    name: 'F#',
    defaultCode: `printfn "Hello, F#!"`,
    prismLang: 'fsharp',
    logo: '🎵'
  },
  {
    id: 86,
    name: 'Zig',
    defaultCode: `const std = @import("std");\n\npub fn main() !void {\n    std.debug.print("Hello, Zig!\\n", .{});\n}`,
    prismLang: 'zig',
    logo: '⚡'
  },
  {
    id: 13,
    name: 'Bash',
    defaultCode: `echo "Hello, Bash!"`,
    prismLang: 'bash',
    logo: '💻'
  },
  {
    id: 12,
    name: 'Assembly',
    defaultCode: `section .data\n    msg db 'Hello, Assembly!', 0\n\nsection .text\n    global _start\n\n_start:\n    ; Write the message to stdout\n    mov eax, 4       ; sys_write syscall number\n    mov ebx, 1       ; stdout file descriptor\n    mov ecx, msg     ; message address\n    mov edx, 16      ; message length\n    int 0x80         ; call the kernel\n\n    ; Exit the program\n    mov eax, 1       ; sys_exit syscall number\n    xor ebx, ebx     ; exit code 0\n    int 0x80         ; call the kernel`,
    prismLang: 'assembly',
    logo: '🔩'
  }
];

// Create a custom black and white theme based on prism themes
const blackAndWhiteTheme = {
  ...themes.nightOwl,
  plain: {
    color: '#FFFFFF',
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
        color: '#FFFFFF',
      },
    },
    {
      types: ['punctuation', 'operator'],
      style: {
        color: '#CCCCCC',
      },
    },
    {
      types: ['entity', 'url', 'symbol', 'number', 'boolean', 'variable', 'constant', 'property', 'regex', 'inserted'],
      style: {
        color: '#EEEEEE',
      },
    },
    {
      types: ['atrule', 'keyword', 'attr-name', 'selector'],
      style: {
        color: '#FFFFFF',
        fontWeight: 'bold',
      },
    },
    {
      types: ['function', 'deleted', 'tag'],
      style: {
        color: '#FFFFFF',
      },
    },
    {
      types: ['function-variable'],
      style: {
        color: '#DDDDDD',
      },
    },
    {
      types: ['tag', 'selector', 'keyword'],
      style: {
        color: '#FFFFFF',
        fontWeight: 'bold',
      },
    },
  ],
};

// Define editor themes
export const EDITOR_THEMES: EditorTheme[] = [
  {
    name: 'Night Owl',
    value: 'night-owl',
    theme: themes.nightOwl,
  },
  {
    name: 'Dracula',
    value: 'dracula',
    theme: themes.dracula,
  },
  {
    name: 'VS Code',
    value: 'vs-code',
    theme: themes.vsDark,
  },
  {
    name: 'Oceanic Next',
    value: 'oceanic-next',
    theme: themes.oceanicNext,
  },
  {
    name: 'Black & White',
    value: 'black-white',
    theme: blackAndWhiteTheme,
  },
];
