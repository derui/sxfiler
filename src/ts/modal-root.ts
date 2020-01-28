import { createContext } from "preact";

/**
   type of modal root.
 */
export type ModalRoot = {
  element: HTMLElement | null;
};

/**
   the context element to get root element for modal
 */
export const ModalRootContext = createContext<ModalRoot>({ element: null });
