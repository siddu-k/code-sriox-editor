
import { useState } from 'react';
import { toast } from 'sonner';
import { ProgrammingLanguage } from '@/types/compiler';
import { LANGUAGES } from '@/constants/languages';

export const useCompiler = () => {
  const [language, setLanguage] = useState<ProgrammingLanguage>(LANGUAGES[0]);
  const [code, setCode] = useState<string>(language.defaultCode);
  const [output, setOutput] = useState<string>('');
  const [isLoading, setIsLoading] = useState<boolean>(false);

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

  const handleCodeCopy = () => {
    navigator.clipboard.writeText(code)
      .then(() => {
        toast.success('Code copied to clipboard!');
      })
      .catch(() => {
        toast.error('Failed to copy code. Please try manually selecting and copying.');
      });
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
      const response = await fetch('https://code.sriox.com/submissions?base64_encoded=true&wait=true', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          language_id: language.id,
          source_code: btoa(code)
        })
      });

      const result = await response.json();
      
      if (result.status?.id >= 6) {
        const stderr = result.stderr ? atob(result.stderr) : '';
        const compileOutput = result.compile_output ? atob(result.compile_output) : '';
        setOutput(`Error: ${result.status.description}\n${stderr || compileOutput || ''}`);
      } else if (result.stdout) {
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

  return {
    language,
    code,
    output,
    isLoading,
    handleLanguageChange,
    handleCodeChange,
    handleCodeCopy,
    handleCodeDownload,
    handleCodeSubmit
  };
};
