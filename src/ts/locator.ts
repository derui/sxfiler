import { Context as ReactContext, createContext } from "react";

import { Context } from "./context";

interface Locator {
  context?: Context;
}

let locator: Locator = {};

/**
 * Set new locator to global context
 * @param newLocator
 */
export function setLocator(newLocator: Locator) {
  locator = newLocator;
}

const LocatorContext: ReactContext<Locator> = createContext(locator);

export default LocatorContext;
