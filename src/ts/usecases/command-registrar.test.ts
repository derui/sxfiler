import { createCommandRegistrar } from "./command-registrar";
import * as AppState from "@/states";
import { CommandLike } from "./type";

describe("Use Case", () => {
  describe("Command Registrar", () => {
    it("regist and execute command", () => {
      const obj = createCommandRegistrar(jest.fn() as any);
      const command = {
        execute: jest.fn(),
      };
      const registed = obj.regist({
        moduleId: "module",
        commandId: "command",
        commandInstance: command,
      });

      const executor = {
        execute: jest.fn(),
      };
      const context = {
        use: () => executor,
      };
      registed.execute("module.command", context, { state: AppState.empty() });
      expect(executor.execute).toBeCalled;
    });

    it("overwrite command when other command registerd with the same name", () => {
      const obj = createCommandRegistrar(jest.fn() as any);
      const command1 = jest.fn() as any;
      const command2 = {
        execute: jest.fn() as any,
      };
      let registed = obj.regist({
        moduleId: "module",
        commandId: "command",
        commandInstance: command1,
      });
      registed = registed.regist({
        moduleId: "module",
        commandId: "command",
        commandInstance: command2,
      });

      const executor = (u: CommandLike) => ({
        execute(arg: any) {
          u.execute(arg);
        },
      });
      const context = {
        use: (u: any) => executor(u),
      };
      registed.execute("module.command", context, { state: AppState.empty() });
      expect(command2.execute).toBeCalled;
    });
  });
});
