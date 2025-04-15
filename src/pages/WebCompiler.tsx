
import React, { useState } from 'react';
import { Button } from '@/components/ui/button';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';

// Language configurations (mock data, can be expanded)
const LANGUAGES = [
  { id: 52, name: 'Python', defaultCode: 'print("Hello, World!")' },
  { id: 63, name: 'JavaScript', defaultCode: 'console.log("Hello, World!");' },
  { id: 50, name: 'C', defaultCode: '#include <stdio.h>\n\nint main() {\n    printf("Hello, World!");\n    return 0;\n}' }
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

  const handleCodeSubmit = async () => {
    setIsLoading(true);
    try {
      // Simulated API call - replace with actual Judge0 API endpoint
      const response = await fetch('https://code.sriox.com/submissions', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          language_id: language.id,
          source_code: code,
          stdin: '' // Optional input
        })
      });

      const result = await response.json();
      setOutput(result.stdout || result.stderr || 'No output');
    } catch (error) {
      setOutput(`Error: ${error instanceof Error ? error.message : 'Unknown error'}`);
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div className="container mx-auto p-4 space-y-4">
      <div className="flex space-x-4">
        <Select onValueChange={handleLanguageChange} defaultValue={language.name}>
          <SelectTrigger className="w-[180px]">
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
          variant="outline"
        >
          {isLoading ? 'Running...' : 'Run Code'}
        </Button>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
        <div className="border rounded p-2">
          <textarea
            className="w-full h-64 font-mono text-sm"
            value={code}
            onChange={(e) => setCode(e.target.value)}
            placeholder="Enter your code here..."
          />
        </div>
        
        <div className="border rounded p-2">
          <pre className="text-sm overflow-auto h-64">
            {output || 'Output will appear here...'}
          </pre>
        </div>
      </div>
    </div>
  );
};

export default WebCompiler;
