
import React, { useState } from 'react';
import { Link } from 'react-router-dom';
import { Button } from '@/components/ui/button';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Download, Settings, LayoutPanelTop, Monitor } from 'lucide-react';
import { Sheet, SheetContent, SheetDescription, SheetHeader, SheetTitle, SheetTrigger } from '@/components/ui/sheet';
import { ResizablePanelGroup, ResizablePanel, ResizableHandle } from '@/components/ui/resizable';
import { LANGUAGES } from '@/constants/languages';
import { useCompiler } from '@/hooks/use-compiler';
import { useCompilerSettings } from '@/hooks/use-compiler-settings';
import CodeEditor from '@/components/compiler/CodeEditor';
import SettingsPanel from '@/components/compiler/SettingsPanel';
import { cn } from '@/lib/utils';

const WebCompiler: React.FC = () => {
  const {
    language,
    code,
    output,
    isLoading,
    handleLanguageChange,
    handleCodeChange,
    handleCodeCopy,
    handleCodeDownload,
    handleCodeSubmit
  } = useCompiler();

  const {
    settings,
    getSize,
    updateUiSize,
    updateEditorTheme,
    toggleClassroomView,
    updatePanelSize
  } = useCompilerSettings();

  const [outputPosition, setOutputPosition] = useState({ x: 20, y: 20 });
  const [isDragging, setIsDragging] = useState(false);
  const outputRef = React.useRef<HTMLDivElement>(null);
  
  // Draggable terminal functionality for classroom view
  const handleMouseDown = (e: React.MouseEvent) => {
    if (!settings.isClassroomView) return;
    setIsDragging(true);
  };

  const handleMouseMove = (e: React.MouseEvent) => {
    if (!isDragging || !settings.isClassroomView) return;
    setOutputPosition({
      x: Math.max(0, Math.min(e.clientX - 100, window.innerWidth - 300)),
      y: Math.max(0, Math.min(e.clientY - 20, window.innerHeight - 200))
    });
  };

  const handleMouseUp = () => {
    setIsDragging(false);
  };

  React.useEffect(() => {
    window.addEventListener('mouseup', handleMouseUp);
    return () => {
      window.removeEventListener('mouseup', handleMouseUp);
    };
  }, []);

  const buttonHeight = `h-${Math.round(getSize(10))}`;
  const buttonPadding = `px-${Math.round(getSize(4))}`;
  const fontSize = `text-${settings.uiSize < 90 ? 'sm' : 'base'}`;

  // Apply the selected theme to the entire compiler
  const themeStyles = {
    backgroundColor: settings.editorTheme.theme.plain.backgroundColor,
    color: settings.editorTheme.theme.plain.color,
    transition: 'background-color 0.3s, color 0.3s'
  };

  return (
    <div 
      className="container mx-auto p-4 space-y-4 min-h-screen"
      style={themeStyles}
      onMouseMove={handleMouseMove}
    >
      <div className="flex items-center justify-between mb-6">
        <h1 className="text-2xl font-bold animate-fade-in">codeer<span className="text-primary/70">.org</span> Code Editor</h1>
        <Link to="/" className="text-sm text-primary/80 hover:text-primary transition-colors">← Back to Home</Link>
      </div>
      
      <div className="flex flex-wrap items-center gap-4 justify-between">
        <div className="flex gap-4 flex-wrap">
          <Select onValueChange={handleLanguageChange} defaultValue={language.name}>
            <SelectTrigger className={`w-[180px] ${buttonHeight} ${fontSize}`} style={{ transform: `scale(${settings.uiSize/100})`, transformOrigin: 'left' }}>
              <SelectValue placeholder="Select Language" />
            </SelectTrigger>
            <SelectContent>
              {LANGUAGES.map(lang => (
                <SelectItem key={lang.id} value={lang.name}>
                  <div className="flex items-center gap-2">
                    <span className="text-lg font-mono">{lang.logo}</span>
                    {lang.name}
                  </div>
                </SelectItem>
              ))}
            </SelectContent>
          </Select>
          
          <Button 
            onClick={handleCodeSubmit} 
            disabled={isLoading}
            variant="default"
            className={`${buttonHeight} ${buttonPadding} ${fontSize} animate-fade-in`}
            style={{ transform: `scale(${settings.uiSize/100})`, transformOrigin: 'left' }}
          >
            {isLoading ? 'Running...' : 'Run Code'}
          </Button>
        </div>

        <div className="flex gap-2">
          <Button
            onClick={toggleClassroomView}
            variant="outline"
            size="icon"
            className={`${buttonHeight} aspect-square animate-fade-in`}
            style={{ transform: `scale(${settings.uiSize/100})`, transformOrigin: 'right' }}
            title={settings.isClassroomView ? "Exit Classroom View" : "Enter Classroom View"}
          >
            <Monitor className="h-4 w-4" />
          </Button>
          
          <Button
            onClick={handleCodeDownload}
            variant="outline"
            size="icon"
            className={`${buttonHeight} aspect-square animate-fade-in`}
            style={{ transform: `scale(${settings.uiSize/100})`, transformOrigin: 'right' }}
          >
            <Download className="h-4 w-4" />
          </Button>
          
          <Sheet>
            <SheetTrigger asChild>
              <Button
                variant="outline"
                size="icon"
                className={`${buttonHeight} aspect-square animate-fade-in`}
                style={{ transform: `scale(${settings.uiSize/100})`, transformOrigin: 'right' }}
              >
                <Settings className="h-4 w-4" />
              </Button>
            </SheetTrigger>
            <SheetContent style={themeStyles}>
              <SheetHeader>
                <SheetTitle>Editor Settings</SheetTitle>
                <SheetDescription>
                  Customize your coding experience
                </SheetDescription>
              </SheetHeader>
              
              <div className="mt-6">
                <SettingsPanel 
                  settings={settings}
                  onUpdateUiSize={updateUiSize}
                  onUpdateTheme={updateEditorTheme}
                  onToggleView={toggleClassroomView}
                  onUpdatePanelSize={updatePanelSize}
                />
              </div>
            </SheetContent>
          </Sheet>
        </div>
      </div>

      {settings.isClassroomView ? (
        <div className="w-full h-[calc(100vh-200px)] animate-fade-in relative">
          <div className="border rounded-lg min-h-[500px] bg-black/90 h-full">
            <CodeEditor
              code={code}
              language={language}
              theme={settings.editorTheme}
              onChange={handleCodeChange}
              onCopy={handleCodeCopy}
            />
          </div>
          
          <div 
            ref={outputRef}
            className={cn(
              "absolute border rounded-lg p-4 bg-black/90 shadow-lg",
              isDragging ? "cursor-grabbing" : "cursor-grab"
            )}
            style={{
              width: '350px',
              maxHeight: '300px',
              left: `${outputPosition.x}px`,
              top: `${outputPosition.y}px`,
              zIndex: 100,
              resize: 'both',
              overflow: 'auto'
            }}
            onMouseDown={handleMouseDown}
          >
            <div className="h-6 bg-black/50 absolute top-0 left-0 right-0 text-xs text-center pt-1">
              Terminal (drag to move)
            </div>
            <pre className="text-sm overflow-auto whitespace-pre-wrap mt-6 text-green-400">
              {output || 'Output will appear here...'}
            </pre>
          </div>
        </div>
      ) : (
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4 animate-fade-in">
          <div className="border rounded-lg p-2 bg-black/90 min-h-[500px]" style={{ flex: settings.panelSize < 50 ? settings.panelSize/100 * 2 : 1 }}>
            <CodeEditor
              code={code}
              language={language}
              theme={settings.editorTheme}
              onChange={handleCodeChange}
              onCopy={handleCodeCopy}
            />
          </div>
          
          <div className="border rounded-lg p-2 bg-black/90" style={{ flex: settings.panelSize > 50 ? (100 - settings.panelSize)/100 * 2 : 1 }}>
            <pre className="text-sm overflow-auto h-[500px] whitespace-pre-wrap p-4 text-green-400">
              {output || 'Output will appear here...'}
            </pre>
          </div>
        </div>
      )}
      
      <div className="mt-8 border-t pt-4 text-center" style={{ borderColor: 'rgba(255,255,255,0.1)' }}>
        <p className="text-sm text-muted-foreground">
          © 2025 <span className="font-medium">codeer.org</span> • Run code in 25+ programming languages
        </p>
      </div>
    </div>
  );
};

export default WebCompiler;
