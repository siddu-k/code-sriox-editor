
import React, { useRef, useEffect } from 'react';
import { Button } from '@/components/ui/button';
import { Copy, Check } from 'lucide-react';
import { Highlight } from 'prism-react-renderer';
import { EditorTheme, ProgrammingLanguage } from '@/types/compiler';
import { toast } from 'sonner';

interface CodeEditorProps {
  code: string;
  language: ProgrammingLanguage;
  theme: EditorTheme;
  onChange: (e: React.ChangeEvent<HTMLTextAreaElement>) => void;
  onCopy: () => void;
}

const CodeEditor: React.FC<CodeEditorProps> = ({
  code,
  language,
  theme,
  onChange,
  onCopy
}) => {
  const [copied, setCopied] = React.useState(false);
  const textAreaRef = useRef<HTMLTextAreaElement>(null);
  const preRef = useRef<HTMLPreElement>(null);
  
  // Sync scrolling between textarea and pre
  useEffect(() => {
    const textArea = textAreaRef.current;
    const pre = preRef.current;
    
    if (!textArea || !pre) return;
    
    const syncScroll = () => {
      pre.scrollTop = textArea.scrollTop;
      pre.scrollLeft = textArea.scrollLeft;
    };
    
    textArea.addEventListener('scroll', syncScroll);
    return () => textArea.removeEventListener('scroll', syncScroll);
  }, []);

  // Focus cursor
  useEffect(() => {
    // Initial focus
    if (textAreaRef.current) {
      const cursorPos = textAreaRef.current.value.length;
      textAreaRef.current.focus();
      textAreaRef.current.setSelectionRange(cursorPos, cursorPos);
    }
  }, [language.id]); // Re-focus when language changes

  const handleCopy = () => {
    navigator.clipboard.writeText(code)
      .then(() => {
        setCopied(true);
        toast.success('Code copied to clipboard!');
        onCopy();
        setTimeout(() => setCopied(false), 2000);
      })
      .catch((err) => {
        console.error('Failed to copy: ', err);
        toast.error('Failed to copy code');
      });
  };

  // Focus handling for better cursor positioning
  const handleFocus = () => {
    if (textAreaRef.current) {
      textAreaRef.current.focus();
    }
  };

  // Tab key handling
  const handleKeyDown = (e: React.KeyboardEvent<HTMLTextAreaElement>) => {
    if (e.key === 'Tab') {
      e.preventDefault();
      const target = e.target as HTMLTextAreaElement;
      const start = target.selectionStart;
      const end = target.selectionEnd;
      
      // Insert 2 spaces at cursor position
      const newValue = target.value.substring(0, start) + '  ' + target.value.substring(end);
      target.value = newValue;
      
      // Move cursor after the inserted tab
      target.selectionStart = target.selectionEnd = start + 2;
      
      // Trigger onChange to update state
      const event = {
        target: target
      } as React.ChangeEvent<HTMLTextAreaElement>;
      
      onChange(event);
    }
  };

  return (
    <div className="relative h-full group" onClick={handleFocus}>
      <textarea
        ref={textAreaRef}
        value={code}
        onChange={onChange}
        onKeyDown={handleKeyDown}
        className="absolute inset-0 w-full h-full p-4 font-mono text-sm resize-none text-transparent caret-white z-10 outline-none bg-transparent"
        style={{ 
          caretColor: theme.theme.plain.color,
          tabSize: 2,
        }}
        spellCheck="false"
        autoCapitalize="none"
        autoCorrect="off"
        autoComplete="off"
        data-gramm="false"
      />
      <Highlight
        theme={theme.theme}
        code={code}
        language={language.prismLang}
      >
        {({ className, style, tokens, getLineProps, getTokenProps }) => (
          <pre 
            ref={preRef}
            className="p-4 overflow-auto h-full m-0 pointer-events-none" 
            style={style}
          >
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
      <div className="absolute top-2 right-2 opacity-0 group-hover:opacity-100 transition-opacity">
        <Button 
          onClick={handleCopy} 
          variant="secondary" 
          size="sm" 
          className="h-7 w-7 p-0"
        >
          {copied ? <Check className="h-3 w-3" /> : <Copy className="h-3 w-3" />}
        </Button>
      </div>
    </div>
  );
};

export default CodeEditor;
