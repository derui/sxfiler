import { useContext } from "preact/hooks";
import { ThemeContext } from "./theme";
import { ModalRootContext } from "./modal-root";

// TODO use theme model instead of string...
export const ModalThemeUpdater = () => {
  const { element } = useContext(ModalRootContext);
  const theme = useContext(ThemeContext);

  if (element && theme) {
    element.classList.add(theme);
  }

  return null;
};
