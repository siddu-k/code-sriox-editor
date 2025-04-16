
import { useState, useEffect } from 'react';
import { useIsMobile } from '@/hooks/use-mobile';
import { EditorTheme, CompilerSettings } from '@/types/compiler';
import { EDITOR_THEMES } from '@/constants/languages';

export const useCompilerSettings = () => {
  const [settings, setSettings] = useState<CompilerSettings>({
    uiSize: 100,
    editorTheme: EDITOR_THEMES[0],
    isClassroomView: false,
    panelSize: 50
  });
  
  const isMobile = useIsMobile();
  
  useEffect(() => {
    if (isMobile) {
      setSettings(prev => ({ ...prev, uiSize: 80 }));
    }
  }, [isMobile]);

  const updateUiSize = (size: number) => {
    setSettings(prev => ({ ...prev, uiSize: size }));
  };

  const updateEditorTheme = (themeName: string) => {
    const selectedTheme = EDITOR_THEMES.find(t => t.value === themeName);
    if (selectedTheme) {
      setSettings(prev => ({ ...prev, editorTheme: selectedTheme }));
    }
  };

  const toggleClassroomView = () => {
    setSettings(prev => ({ ...prev, isClassroomView: !prev.isClassroomView }));
  };

  const updatePanelSize = (size: number) => {
    setSettings(prev => ({ ...prev, panelSize: size }));
  };

  const getSize = (baseSize: number) => {
    const scale = settings.uiSize / 100;
    return Math.max(baseSize * scale, baseSize * 0.6);
  };

  return {
    settings,
    getSize,
    updateUiSize,
    updateEditorTheme,
    toggleClassroomView,
    updatePanelSize
  };
};
