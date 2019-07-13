import { Actions } from "@/actions";
import { actions } from "@/actions/filer";
import { CommandLike } from "@/usecases/type";
import { Dispatcher } from "@/types";
import { CommandRegistrar } from "@/usecases/command-registrar";
import { Apis } from "@/apis";

const belongingModuleId = "builtin";
const commandId = "filer.moveParent";

/**
 * Regist command instance to the registrar
 */
export const registCommand = (registrar: CommandRegistrar) =>
  registrar.regist({
    moduleId: belongingModuleId,
    commandId,
    commandInstance: createCommand(),
  });

export const createCommand = (): CommandLike => {
  return {
    async execute(dispatch: Dispatcher<Actions>, args) {
      if (!args) {
        throw new Error("Do not take store state");
      }
      const { state, client } = args;

      const side = state.fileList.currentSide;
      const filer = await client.call(Apis.Filer.MoveParent, side);

      if (!filer) {
        return;
      }

      dispatch.dispatch(actions.load({ filer }));
    },
  };
};
