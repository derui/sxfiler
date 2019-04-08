import { createCommandRegistrar } from "./command-registrar";

describe("Use Case", () => {
  describe("Command Registrar", () => {
    it("create new registrar", () => {
      const obj = createCommandRegistrar();

      expect(obj.findCommand("command")).toBeUndefined;
    });

    it("regist and find command", () => {
      const obj = createCommandRegistrar();
      const registed = obj.regist({
        moduleId: "module",
        commandId: "command",
        commandInstance: jest.fn() as any,
      });

      expect(registed.findCommand("module.command")).not.toBeUndefined;
    });

    it("overwrite command when other command registerd with the same name", () => {
      const obj = createCommandRegistrar();
      const command1 = jest.fn() as any;
      const command2 = jest.fn() as any;
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

      expect(registed.findCommand("module.command")).toBe(command2);
    });
  });
});
