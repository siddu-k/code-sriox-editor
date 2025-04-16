
import React from 'react';
import { Button } from '@/components/ui/button';
import { Copy } from 'lucide-react';
import { Highlight } from 'prism-react-renderer';
import { EditorTheme, ProgrammingLanguage } from '@/types/compiler';

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
  return (
    <div className="relative h-full group">
      <textarea
        value={code}
        onChange={onChange}
        className="absolute inset-0 w-full h-full p-4 font-mono text-sm resize-none bg-transparent text-transparent caret-white z-10"
        spellCheck="false"
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
          onClick={onCopy} 
          variant="secondary" 
          size="sm" 
          className="h-7 w-7 p-0"
        >
          <Copy className="h-3 w-3" />
        </Button>
      </div>
    </div>
  );
};

export default CodeEditor;
