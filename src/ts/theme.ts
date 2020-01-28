import { createContext } from "preact";
import { useContext } from "preact/hooks";

// The context to get theme. The implementation of theme in this project is simple CSS class name for DOM.
export const ThemeContext = createContext<string | undefined>(undefined);

// too simple hook to use theme in container component
export const useTheme = () => {
  return useContext(ThemeContext);
};
