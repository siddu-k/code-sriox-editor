
import { themes } from 'prism-react-renderer';

export interface ProgrammingLanguage {
  id: number;
  name: string;
  defaultCode: string;
  prismLang: string;
  logo: string;
}

export interface EditorTheme {
  name: string;
  value: string;
  theme: typeof themes.nightOwl;
}

export interface CompilerSettings {
  uiSize: number;
  editorTheme: EditorTheme;
  isClassroomView: boolean;
  panelSize: number;
}
