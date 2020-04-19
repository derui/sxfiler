import { createContext } from "preact";

import { ContextLike } from "./context";
import * as CommandResolver from "./commands/command-resolver";
import * as CommandExecutor from "./commands/command-executor";
import * as EventHub from "./typed-event-hub";

export type Locator = {
  readonly context?: ContextLike;
  readonly commandExecutor?: CommandExecutor.Type;
  readonly commandResolver?: CommandResolver.Type;
  readonly eventHub?: EventHub.Type;
};

let locator: Locator = {};

/**
 * Set new locator to global context
 * @param newLocator
 */
export const setLocator = (newLocator: Locator) => {
  Object.assign(locator, newLocator);
};

export const LocatorContext = createContext(locator);
