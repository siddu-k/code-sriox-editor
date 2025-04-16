
import React, { useState } from 'react';
import { Link } from 'react-router-dom';
import { Button } from '@/components/ui/button';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { toast } from "sonner";
import { Download } from 'lucide-react';
import { Highlight, themes } from 'prism-react-renderer';

// Extended language configurations with all Judge0 supported languages
const LANGUAGES = [
  { id: 71, name: 'Python', defaultCode: 'print("Hello, World!")', prismLang: 'python' },
  { id: 63, name: 'JavaScript', defaultCode: 'console.log("Hello, World!");', prismLang: 'javascript' },
  { id: 54, name: 'C++', defaultCode: '#include <iostream>\n\nint main() {\n    std::cout << "Hello, World!";\n    return 0;\n}', prismLang: 'cpp' },
  { id: 62, name: 'Java', defaultCode: 'public class Main {\n    public static void main(String[] args) {\n        System.out.println("Hello, World!");\n    }\n}', prismLang: 'java' },
  { id: 51, name: 'C', defaultCode: '#include <stdio.h>\n\nint main() {\n    printf("Hello, World!");\n    return 0;\n}', prismLang: 'c' },
  { id: 60, name: 'Go', defaultCode: 'package main\n\nimport "fmt"\n\nfunc main() {\n    fmt.Println("Hello, World!")\n}', prismLang: 'go' },
  { id: 78, name: 'Kotlin', defaultCode: 'fun main() {\n    println("Hello, World!")\n}', prismLang: 'kotlin' },
  { id: 72, name: 'Ruby', defaultCode: 'puts "Hello, World!"', prismLang: 'ruby' },
  { id: 74, name: 'TypeScript', defaultCode: 'console.log("Hello, World!");', prismLang: 'typescript' },
  { id: 82, name: 'SQL', defaultCode: 'SELECT "Hello, World!" as message;', prismLang: 'sql' },
  { id: 50, name: 'C#', defaultCode: 'using System;\n\nclass Program {\n    static void Main() {\n        Console.WriteLine("Hello, World!");\n    }\n}', prismLang: 'csharp' },
  { id: 68, name: 'PHP', defaultCode: '<?php\necho "Hello, World!";\n?>', prismLang: 'php' },
  { id: 73, name: 'Rust', defaultCode: 'fn main() {\n    println!("Hello, World!");\n}', prismLang: 'rust' },
  { id: 83, name: 'Swift', defaultCode: 'print("Hello, World!")', prismLang: 'swift' },
  { id: 52, name: 'Clojure', defaultCode: '(println "Hello, World!")', prismLang: 'clojure' },
  { id: 61, name: 'Haskell', defaultCode: 'main = putStrLn "Hello, World!"', prismLang: 'haskell' },
  { id: 79, name: 'Objective-C', defaultCode: '#import <Foundation/Foundation.h>\n\nint main() {\n    @autoreleasepool {\n        NSLog(@"Hello, World!");\n    }\n    return 0;\n}', prismLang: 'objectivec' },
  { id: 67, name: 'Pascal', defaultCode: 'program HelloWorld;\nbegin\n  writeln(\'Hello, World!\');\nend.', prismLang: 'pascal' },
  { id: 85, name: 'Perl', defaultCode: 'print "Hello, World!\\n";', prismLang: 'perl' },
  { id: 75, name: 'R', defaultCode: 'cat("Hello, World!")', prismLang: 'r' }
];

const WebCompiler: React.FC = () => {
  const [language, setLanguage] = useState(LANGUAGES[0]);
  const [code, setCode] = useState(language.defaultCode);
  const [output, setOutput] = useState('');
  const [isLoading, setIsLoading] = useState(false);

  const handleLanguageChange = (langName: string) => {
    const selectedLang = LANGUAGES.find(l => l.name === langName);
    if (selectedLang) {
      setLanguage(selectedLang);
      setCode(selectedLang.defaultCode);
    }
  };

  const handleCodeChange = (e: React.ChangeEvent<HTMLTextAreaElement>) => {
    setCode(e.target.value);
  };

  const handleCodeDownload = () => {
    const blob = new Blob([code], { type: 'text/plain' });
    const url = window.URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `code.${language.prismLang}`;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    window.URL.revokeObjectURL(url);
    toast.success('Code downloaded successfully!');
  };

  const handleCodeSubmit = async () => {
    setIsLoading(true);
    setOutput('Running your code...');
    
    try {
      // Make a single API call with base64_encoded=true
      const response = await fetch('https://code.sriox.com/submissions?base64_encoded=true&wait=true', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          language_id: language.id,
          source_code: btoa(code) // Base64 encode the source code
        })
      });

      const result = await response.json();
      
      // Handle the result
      if (result.status?.id >= 6) { // Error statuses start at 6
        // Decode base64 output
        const stderr = result.stderr ? atob(result.stderr) : '';
        const compileOutput = result.compile_output ? atob(result.compile_output) : '';
        setOutput(`Error: ${result.status.description}\n${stderr || compileOutput || ''}`);
      } else if (result.stdout) {
        // Decode base64 output
        setOutput(atob(result.stdout));
      } else if (result.stderr) {
        setOutput(atob(result.stderr));
      } else if (result.compile_output) {
        setOutput(atob(result.compile_output));
      } else {
        setOutput('No output');
      }
    } catch (error) {
      console.error('Submission error:', error);
      setOutput(`Error: ${error instanceof Error ? error.message : 'Unknown error'}`);
      toast.error("Failed to run code. Please try again.");
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div className="container mx-auto p-4 space-y-4 bg-background text-foreground min-h-screen">
      <div className="flex items-center justify-between mb-6">
        <h1 className="text-2xl font-bold">codeer<span className="text-primary/70">.org</span> Code Editor</h1>
        <Link to="/" className="text-sm text-primary/80 hover:text-primary transition-colors">← Back to Home</Link>
      </div>
      
      <div className="flex flex-wrap items-center gap-4 justify-between">
        <div className="flex gap-4 flex-wrap">
          <Select onValueChange={handleLanguageChange} defaultValue={language.name}>
            <SelectTrigger className="w-[180px] bg-background">
              <SelectValue placeholder="Select Language" />
            </SelectTrigger>
            <SelectContent>
              {LANGUAGES.map(lang => (
                <SelectItem key={lang.id} value={lang.name}>
                  {lang.name}
                </SelectItem>
              ))}
            </SelectContent>
          </Select>
          
          <Button 
            onClick={handleCodeSubmit} 
            disabled={isLoading}
            variant="default"
          >
            {isLoading ? 'Running...' : 'Run Code'}
          </Button>
        </div>

        <Button
          onClick={handleCodeDownload}
          variant="outline"
          className="gap-2"
        >
          <Download className="h-4 w-4" />
          Download Code
        </Button>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
        <div className="border rounded-lg p-2 bg-black/90 min-h-[500px]">
          <div className="relative h-full">
            <textarea
              value={code}
              onChange={handleCodeChange}
              className="absolute inset-0 w-full h-full p-4 font-mono text-sm resize-none bg-transparent text-transparent caret-white z-10"
              spellCheck="false"
            />
            <Highlight
              theme={themes.nightOwl}
              code={code}
              language={language.prismLang}
            >
              {({ className, style, tokens, getLineProps, getTokenProps }) => (
                <pre className="p-4 overflow-auto h-full m-0 pointer-events-none" style={style}>
                  {tokens.map((line, i) => (
                    <div key={i} {...getLineProps({ line })}>
                      <span className="text-gray-500 mr-4 select-none">
                        {(i + 1).toString().padStart(2, '0')}
                      </span>
                      {line.map((token, key) => (
                        <span key={key} {...getTokenProps({ token })} />
                      ))}
                    </div>
                  ))}
                </pre>
              )}
            </Highlight>
          </div>
        </div>
        
        <div className="border rounded-lg p-2 bg-black/90">
          <pre className="text-sm overflow-auto h-[500px] whitespace-pre-wrap p-4 text-green-400">
            {output || 'Output will appear here...'}
          </pre>
        </div>
      </div>
      
      <div className="mt-8 border-t border-border pt-4 text-center">
        <p className="text-sm text-muted-foreground">
          © 2025 <span className="font-medium">codeer.org</span> • Run code in 20+ programming languages
        </p>
      </div>
    </div>
  );
};

export default WebCompiler;
