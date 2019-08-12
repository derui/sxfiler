import { Context as ReactContext, createContext } from "react";

import { ApiMethod } from "./apis";
import { Client } from "./libs/json-rpc/client";
import { ContextLike } from "./context";
import { CommandRegistrar } from "./usecases/command-registrar";

export type Locator = {
  readonly context?: ContextLike;
  readonly client?: Client<ApiMethod>;
  readonly commandRegistrar?: CommandRegistrar;
};

let locator: Locator = {};

/**
 * Set new locator to global context
 * @param newLocator
 */
export const setLocator = function setLocator(newLocator: Locator) {
  Object.assign(locator, newLocator);
};

export const LocatorContext: ReactContext<Locator> = createContext(locator);
