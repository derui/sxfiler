import { Context as ReactContext, createContext } from "react";

import { ContextLike } from "./context";

export type Locator = {
  context?: ContextLike;
};

let locator: Locator = {};

/**
 * Set new locator to global context
 * @param newLocator
 */
export function setLocator(newLocator: Locator): void {
  Object.assign(locator, newLocator);
}

const LocatorContext: ReactContext<Locator> = createContext(locator);

export default LocatorContext;
