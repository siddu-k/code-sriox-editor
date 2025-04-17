
import React from 'react';
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Button } from '@/components/ui/button';
import { Slider } from '@/components/ui/slider';
import { LayoutPanelTop, Terminal } from 'lucide-react';
import { EDITOR_THEMES } from '@/constants/languages';
import { CompilerSettings } from '@/types/compiler';
import { Switch } from '@/components/ui/switch';
import { Label } from '@/components/ui/label';

interface SettingsPanelProps {
  settings: CompilerSettings;
  onUpdateUiSize: (size: number) => void;
  onUpdateTheme: (theme: string) => void;
  onToggleView: () => void;
  onUpdatePanelSize: (size: number) => void;
  onToggleTerminal: () => void;
}

const SettingsPanel: React.FC<SettingsPanelProps> = ({
  settings,
  onUpdateUiSize,
  onUpdateTheme,
  onToggleView,
  onUpdatePanelSize,
  onToggleTerminal
}) => {
  return (
    <Tabs defaultValue="general" className="w-full">
      <TabsList className="grid grid-cols-3 mb-4">
        <TabsTrigger value="general">General</TabsTrigger>
        <TabsTrigger value="appearance">Appearance</TabsTrigger>
        <TabsTrigger value="layout">Layout</TabsTrigger>
      </TabsList>
      
      <TabsContent value="general" className="space-y-4">
        <div>
          <h3 className="mb-2 text-sm font-medium">UI Size</h3>
          <Slider
            defaultValue={[settings.uiSize]}
            max={120}
            min={60}
            step={5}
            onValueChange={(value) => onUpdateUiSize(value[0])}
            className="py-4"
          />
          <div className="flex justify-between text-xs text-muted-foreground">
            <span>Small</span>
            <span>Default</span>
            <span>Large</span>
          </div>
        </div>
        
        <div className="flex items-center justify-between">
          <div className="space-y-0.5">
            <Label htmlFor="terminal-toggle">Terminal Visibility</Label>
            <div className="text-xs text-muted-foreground">Show or hide the terminal</div>
          </div>
          <Switch
            id="terminal-toggle"
            checked={settings.isTerminalVisible}
            onCheckedChange={onToggleTerminal}
          />
        </div>
      </TabsContent>
      
      <TabsContent value="appearance" className="space-y-4">
        <div>
          <h3 className="mb-2 text-sm font-medium">Editor Theme</h3>
          <Select 
            defaultValue={settings.editorTheme.value} 
            onValueChange={onUpdateTheme}
          >
            <SelectTrigger className="w-full">
              <SelectValue placeholder="Select theme" />
            </SelectTrigger>
            <SelectContent>
              {EDITOR_THEMES.map((theme) => (
                <SelectItem key={theme.value} value={theme.value}>
                  {theme.name}
                </SelectItem>
              ))}
            </SelectContent>
          </Select>
        </div>
      </TabsContent>
      
      <TabsContent value="layout" className="space-y-4">
        <div>
          <h3 className="mb-2 text-sm font-medium">Panel Split</h3>
          <Slider
            defaultValue={[settings.panelSize]}
            max={80}
            min={20}
            step={5}
            onValueChange={(value) => onUpdatePanelSize(value[0])}
            className="py-4"
          />
          <div className="flex justify-between text-xs text-muted-foreground">
            <span>More Code</span>
            <span>Equal</span>
            <span>More Output</span>
          </div>
        </div>
        
        <div>
          <h3 className="mb-2 text-sm font-medium">View Mode</h3>
          <Button 
            onClick={onToggleView}
            variant={settings.isClassroomView ? "default" : "outline"}
            className="w-full"
          >
            <LayoutPanelTop className="mr-2 h-4 w-4" />
            {settings.isClassroomView ? "Exit Classroom View" : "Enter Classroom View"}
          </Button>
        </div>
      </TabsContent>
    </Tabs>
  );
};

export default SettingsPanel;
