import { create } from "./command-executor";
import { CommandLike } from "./type";

describe("Commands", () => {
  describe("Command Executor", () => {
    it("execute command with command", () => {
      const context = {
        use: (command$: CommandLike<any>) => {
          expect(command$).toBe(command);
          return executor;
        },
      };

      const obj = create(jest.fn() as any, context);
      const command = {
        identifier: "identifier",
        execute: jest.fn(),
      };

      const executor = jest.fn();

      obj.execute(() => command, {} as any, null);
      expect(executor).toBeCalled();
    });
  });
});
