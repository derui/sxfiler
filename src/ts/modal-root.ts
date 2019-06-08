import { Context as ReactContext, createContext } from "react";

/**
   type of modal root.
 */
export type ModalRoot = {
  element?: HTMLElement;
};

/**
   the context element to get root element for modal
 */
const ModalRootContext: ReactContext<ModalRoot> = createContext({});

export default ModalRootContext;
