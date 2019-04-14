import { createCommandRegistrar } from "./command-registrar";

describe("Use Case", () => {
  describe("Command Registrar", () => {
    it("do not execute command if it is not found", () => {
      const obj = createCommandRegistrar(jest.fn() as any);
      const context = {
        execute: jest.fn(),
      };

      obj.execute("command", context);
      expect(context.execute).not.toBeCalled;
    });

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

      const context = {
        execute: jest.fn(),
      };
      registed.execute("module.command", context);
      expect(context.execute).toBeCalled;
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

      const context = {
        execute: jest.fn(),
      };
      registed.execute("module.command", context);
      expect(command2.execute).toBeCalled;
    });
  });
});
