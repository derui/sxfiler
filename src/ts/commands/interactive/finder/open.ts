import { Actions } from "@/modules";
import { CommandLike, CommandState, CommandDescriptor } from "@/commands/type";
import { Dispatcher } from "@/types";
import { currentSideItemsSelector } from "@/modules/filer/selectors";
import { actions } from "@/modules/completer";
import { actions as keymapActions } from "@/modules/keymap";
import * as Procs from "@/rpc/client-procedures";
import { InitializeRequest, Item, Candidate } from "@/generated/completer_pb";
import { also } from "@/libs/fn";
import { UIContext } from "@/types/ui-context";

const identifier = "interactive.finder.open";

export type Payload = undefined;
export type Command = CommandLike<Payload>;

/**
 * command descriptor to resolve
 */
export const descriptor: CommandDescriptor<Payload> = Object.freeze({
  identifier,
  payloadSpec: undefined as any,
});

export const createCommand = (): Command => {
  return {
    identifier,
    async execute(dispatcher: Dispatcher<Actions>, args: CommandState) {
      const source = currentSideItemsSelector(args.state.filer).map((v) =>
        also(new Item(), (item) => {
          item.setId(v.getId());
          item.setValue(v.getName());
        })
      );

      const initialCandidates = source.map((v) => {
        const candidate = new Candidate();
        candidate.setStart(0);
        candidate.setLength(0);
        candidate.setValue(v.clone());
        return candidate;
      });
      dispatcher.dispatch(actions.updateCandidates(initialCandidates));

      const request = new InitializeRequest();
      request.setSourceList(source);
      await args.clientResolver.rpcClient().use(Procs.Completer.initialize)(request);

      dispatcher.dispatch(actions.open("Find items"));
      dispatcher.dispatch(keymapActions.replaceContext([UIContext.OnCompletion, UIContext.ForFinder]));
    },
  };
};
