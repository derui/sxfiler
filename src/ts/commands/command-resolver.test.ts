import { create } from "./command-resolver";

describe("Commands", () => {
  describe("Command Resolver", () => {
    test("resolve by command descriptor", () => {
      const descriptor = {
        identifier: "identifier",
        payloadSpec: {},
      };

      const commandFactory = jest.fn();
      const resolver = create();
      resolver.register({ factory: commandFactory, descriptor });

      const actual = resolver.resolveBy(descriptor);
      expect(actual).toBe(commandFactory);
    });

    test("return undefined if factory did not register", () => {
      const descriptor = {
        identifier: "identifier",
        payloadSpec: {},
      };

      const factory = jest.fn();
      const resolver = create();
      resolver.register({ descriptor, factory });
      const didNotRegisterDesc = {
        identifier: "diff ident",
        payloadSpec: {},
      };

      expect(resolver.resolveBy(didNotRegisterDesc)).toBeUndefined();
    });
  });
});
