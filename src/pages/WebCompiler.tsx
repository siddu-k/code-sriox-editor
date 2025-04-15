
import React, { useState } from 'react';
import { Button } from '@/components/ui/button';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Textarea } from '@/components/ui/textarea';
import { toast } from "sonner";

// Language configurations
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

  const checkSubmissionStatus = async (token: string): Promise<any> => {
    try {
      const response = await fetch(`https://code.sriox.com/submissions/${token}?base64_encoded=false`, {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json',
        }
      });
      return await response.json();
    } catch (error) {
      console.error('Error checking submission status:', error);
      throw error;
    }
  };

  const handleCodeSubmit = async () => {
    setIsLoading(true);
    setOutput('Running your code...');
    
    try {
      // Step 1: Create submission
      const submitResponse = await fetch('https://code.sriox.com/submissions', {
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

      const submitResult = await submitResponse.json();
      
      if (!submitResult.token) {
        throw new Error('No submission token received');
      }
      
      // Step 2: Poll for results
      let result = await checkSubmissionStatus(submitResult.token);
      let attempts = 0;
      const maxAttempts = 10;
      
      // Poll until the status is not "Processing" or we reach max attempts
      while (
        result.status?.description === "Processing" && 
        attempts < maxAttempts
      ) {
        await new Promise(resolve => setTimeout(resolve, 1000)); // Wait 1 second
        result = await checkSubmissionStatus(submitResult.token);
        attempts++;
      }

      // Handle the result
      if (result.status?.id >= 6) { // Error statuses start at 6
        setOutput(`Error: ${result.status.description}\n${result.stderr || ''}`);
      } else if (result.stdout) {
        setOutput(result.stdout);
      } else if (result.stderr) {
        setOutput(result.stderr);
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
    <div className="container mx-auto p-4 space-y-4">
      <div className="flex flex-wrap items-center gap-4">
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
          variant="default"
        >
          {isLoading ? 'Running...' : 'Run Code'}
        </Button>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
        <div className="border rounded p-2 bg-background">
          <Textarea
            className="w-full h-64 font-mono text-sm resize-none"
            value={code}
            onChange={(e) => setCode(e.target.value)}
            placeholder="Enter your code here..."
          />
        </div>
        
        <div className="border rounded p-2 bg-background">
          <pre className="text-sm overflow-auto h-64 whitespace-pre-wrap p-2">
            {output || 'Output will appear here...'}
          </pre>
        </div>
      </div>
    </div>
  );
};

export default WebCompiler;
