import { Context as ReactContext, createContext } from "react";

import { ContextLike } from "./context";
import { CommandRegistrar, createCommandRegistrar } from "./usecases/command-registrar";

export type Locator = {
  context?: ContextLike;
  commandRegistrar: CommandRegistrar;
};

let locator: Locator = { commandRegistrar: createCommandRegistrar() };

/**
 * Set new locator to global context
 * @param newLocator
 */
export function setLocator(newLocator: Locator): void {
  Object.assign(locator, newLocator);
}

const LocatorContext: ReactContext<Locator> = createContext(locator);

export default LocatorContext;
