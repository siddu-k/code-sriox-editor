
import React, { useRef } from 'react';
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

  const handleCopy = () => {
    navigator.clipboard.writeText(code).then(() => {
      setCopied(true);
      toast.success('Code copied to clipboard!');
      onCopy();
      setTimeout(() => setCopied(false), 2000);
    });
  };

  // Focus handling for better cursor positioning
  const handleFocus = () => {
    if (textAreaRef.current) {
      textAreaRef.current.focus();
    }
  };

  return (
    <div className="relative h-full group" onClick={handleFocus}>
      <textarea
        ref={textAreaRef}
        value={code}
        onChange={onChange}
        className="absolute inset-0 w-full h-full p-4 font-mono text-sm resize-none bg-transparent text-transparent caret-white z-10 outline-none"
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
