import { Actions } from "@/actions";
import * as actions from "@/actions/key-map";
import { CommandLike } from "@/usecases/type";
import { Dispatcher } from "@/types";
import { CommandRegistrar } from "@/usecases/command-registrar";
import { Apis } from "@/apis";

const belongingModuleId = "builtin";
const commandId = "keymap.reload";

/**
 * Regist command instance to the registrar
 */
export const registCommand = function registCommand(registrar: CommandRegistrar) {
  return registrar.regist({
    moduleId: belongingModuleId,
    commandId,
    commandInstance: createCommand(),
  });
};

export const createCommand = function createCommand(): CommandLike {
  return {
    async execute(dispatch: Dispatcher<Actions>, args) {
      if (!args) {
        throw new Error("Do not take store state");
      }
      const { client } = args;

      const keymap = await client.call(Apis.Keymap.Reload, {});

      dispatch.dispatch(actions.updateKeymap(keymap));
    },
  };
};
