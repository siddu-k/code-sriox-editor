
import React from 'react';
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

  const buttonHeight = `h-${Math.round(getSize(10))}`;
  const buttonPadding = `px-${Math.round(getSize(4))}`;
  const fontSize = `text-${settings.uiSize < 90 ? 'sm' : 'base'}`;

  return (
    <div className="container mx-auto p-4 space-y-4 bg-background text-foreground min-h-screen">
      <div className="flex items-center justify-between mb-6">
        <h1 className="text-2xl font-bold animate-fade-in">codeer<span className="text-primary/70">.org</span> Code Editor</h1>
        <Link to="/" className="text-sm text-primary/80 hover:text-primary transition-colors">← Back to Home</Link>
      </div>
      
      <div className="flex flex-wrap items-center gap-4 justify-between">
        <div className="flex gap-4 flex-wrap">
          <Select onValueChange={handleLanguageChange} defaultValue={language.name}>
            <SelectTrigger className={`w-[180px] bg-background ${buttonHeight} ${fontSize}`} style={{ transform: `scale(${settings.uiSize/100})`, transformOrigin: 'left' }}>
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
            <SheetContent>
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
        <div className="w-full h-[calc(100vh-200px)] animate-fade-in">
          <ResizablePanelGroup
            direction="vertical"
            className="border rounded-lg min-h-[500px] bg-black/90"
          >
            <ResizablePanel defaultSize={70} minSize={30}>
              <CodeEditor
                code={code}
                language={language}
                theme={settings.editorTheme}
                onChange={handleCodeChange}
                onCopy={handleCodeCopy}
              />
            </ResizablePanel>
            <ResizableHandle />
            <ResizablePanel defaultSize={30}>
              <pre className="text-sm overflow-auto h-full whitespace-pre-wrap p-4 text-green-400">
                {output || 'Output will appear here...'}
              </pre>
            </ResizablePanel>
          </ResizablePanelGroup>
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
      
      <div className="mt-8 border-t border-border pt-4 text-center">
        <p className="text-sm text-muted-foreground">
          © 2025 <span className="font-medium">codeer.org</span> • Run code in 25+ programming languages
        </p>
      </div>
    </div>
  );
};

export default WebCompiler;
