import * as CommandResolver from "./command-resolver";
import * as internal from "./internal";
import * as interactive from "./interactive";
import { Type } from "./command-resolver";

export const makeWiredResolver = (): Type => {
  const resolver = CommandResolver.create();

  //#COMMAND REGISTER
  internal.registerToResolver(resolver);
  interactive.registerToResolver(resolver);

  return resolver;
};
