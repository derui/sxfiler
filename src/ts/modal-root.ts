import { createContext } from "react";

/**
   type of modal root.
 */
export type ModalRoot = {
  element: HTMLElement | null;
};

/**
   the context element to get root element for modal
 */
const ModalRootContext = createContext<ModalRoot>({ element: null });

export default ModalRootContext;
