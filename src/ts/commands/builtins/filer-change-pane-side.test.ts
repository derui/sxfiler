import * as C from "./filer-change-pane-side";
import * as AppState from "@/states";
import * as actions from "@/actions/filer";
import { createResolverMocks } from "../client-resolver-mock";

describe("Commands", () => {
  describe("Filer", () => {
    describe("Change the side of pane to use ", () => {
      const { dispatcher, clientResolver } = createResolverMocks();
      afterEach(jest.clearAllMocks);

      it("call dispatch function", async () => {
        const command = C.createCommand();

        await command.execute(dispatcher);
        expect(dispatcher.dispatch).toBeCalled();
      });

      it("change the side of pane", async () => {
        const command = C.createCommand();
        const state = AppState.empty();

        await command.execute(dispatcher, { state: state, clientResolver });
        expect(dispatcher.dispatch).toBeCalledWith(actions.changeSide());
      });
    });
  });
});
