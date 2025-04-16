import { ProgrammingLanguage, EditorTheme } from '@/types/compiler';
import { themes } from 'prism-react-renderer';

export const LANGUAGES: ProgrammingLanguage[] = [
  { id: 71, name: 'Python', defaultCode: 'print("Hello, World!")', prismLang: 'python', logo: '🐍' },
  { id: 63, name: 'JavaScript', defaultCode: 'console.log("Hello, World!");', prismLang: 'javascript', logo: 'JS' },
  { id: 54, name: 'C++', defaultCode: '#include <iostream>\n\nint main() {\n    std::cout << "Hello, World!";\n    return 0;\n}', prismLang: 'cpp', logo: 'C++' },
  { id: 62, name: 'Java', defaultCode: 'public class Main {\n    public static void main(String[] args) {\n        System.out.println("Hello, World!");\n    }\n}', prismLang: 'java', logo: '☕' },
  { id: 51, name: 'C', defaultCode: '#include <stdio.h>\n\nint main() {\n    printf("Hello, World!");\n    return 0;\n}', prismLang: 'c', logo: 'C' },
  { id: 60, name: 'Go', defaultCode: 'package main\n\nimport "fmt"\n\nfunc main() {\n    fmt.Println("Hello, World!")\n}', prismLang: 'go', logo: '🐹' },
  { id: 78, name: 'Kotlin', defaultCode: 'fun main() {\n    println("Hello, World!")\n}', prismLang: 'kotlin', logo: 'KT' },
  { id: 72, name: 'Ruby', defaultCode: 'puts "Hello, World!"', prismLang: 'ruby', logo: '💎' },
  { id: 74, name: 'TypeScript', defaultCode: 'console.log("Hello, World!");', prismLang: 'typescript', logo: 'TS' },
  { id: 82, name: 'SQL', defaultCode: 'SELECT "Hello, World!" as message;', prismLang: 'sql', logo: '🔍' },
  { id: 50, name: 'C#', defaultCode: 'using System;\n\nclass Program {\n    static void Main() {\n        Console.WriteLine("Hello, World!");\n    }\n}', prismLang: 'csharp', logo: 'C#' },
  { id: 68, name: 'PHP', defaultCode: '<?php\necho "Hello, World!";\n?>', prismLang: 'php', logo: '🐘' },
  { id: 73, name: 'Rust', defaultCode: 'fn main() {\n    println!("Hello, World!");\n}', prismLang: 'rust', logo: '🦀' },
  { id: 83, name: 'Swift', defaultCode: 'print("Hello, World!")', prismLang: 'swift', logo: '🐦' },
  { id: 52, name: 'Clojure', defaultCode: '(println "Hello, World!")', prismLang: 'clojure', logo: '()' },
  { id: 61, name: 'Haskell', defaultCode: 'main = putStrLn "Hello, World!"', prismLang: 'haskell', logo: 'λ' },
  { id: 79, name: 'Objective-C', defaultCode: '#import <Foundation/Foundation.h>\n\nint main() {\n    @autoreleasepool {\n        NSLog(@"Hello, World!");\n    }\n    return 0;\n}', prismLang: 'objectivec', logo: 'OC' },
  { id: 67, name: 'Pascal', defaultCode: 'program HelloWorld;\nbegin\n  writeln(\'Hello, World!\');\nend.', prismLang: 'pascal', logo: 'P' },
  { id: 85, name: 'Perl', defaultCode: 'print "Hello, World!\\n";', prismLang: 'perl', logo: '🐪' },
  { id: 75, name: 'R', defaultCode: 'cat("Hello, World!")', prismLang: 'r', logo: 'R' },
  { id: 80, name: 'Scala', defaultCode: 'object Main extends App {\n  println("Hello, World!")\n}', prismLang: 'scala', logo: 'SC' },
  { id: 55, name: 'Lisp', defaultCode: '(format t "Hello, World!")', prismLang: 'lisp', logo: 'L' },
  { id: 77, name: 'Bash', defaultCode: 'echo "Hello, World!"', prismLang: 'bash', logo: '💻' },
  { id: 81, name: 'Erlang', defaultCode: 'main(_) ->\n    io:fwrite("Hello, World!\\n").', prismLang: 'erlang', logo: 'ER' },
  { id: 57, name: 'Elixir', defaultCode: 'IO.puts "Hello, World!"', prismLang: 'elixir', logo: '💧' }
];

export const EDITOR_THEMES: EditorTheme[] = [
  { name: 'Night Owl', value: 'nightOwl', theme: themes.nightOwl },
  { name: 'Dracula', value: 'dracula', theme: themes.dracula },
  { name: 'VS Dark', value: 'vsDark', theme: themes.vsDark },
  { name: 'One Dark', value: 'oneDark', theme: themes.oneDark },
];
