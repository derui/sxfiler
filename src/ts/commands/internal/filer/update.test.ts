import * as C from "./update";
import * as M from "@/commands/client-resolver-mock";
import { Filer } from "@/generated/filer_pb";
import { actions } from "@/modules/filer";

describe("Commands", () => {
  describe("internal:filer:Update", () => {
    test("dispatch update action for filer", async () => {
      const mocks = M.createResolverMocks();
      const filer = new Filer();

      await C.createCommand().execute(mocks.dispatcher, {} as any, { filer });
      expect(mocks.dispatcher.dispatch).toBeCalledWith(actions.update(filer));
    });

    test("do not dispatch action when payload is not passed", async () => {
      const mocks = M.createResolverMocks();

      await C.createCommand().execute(mocks.dispatcher, {} as any, {});
      expect(mocks.dispatcher.dispatch).not.toBeCalled();
    });
  });
});
