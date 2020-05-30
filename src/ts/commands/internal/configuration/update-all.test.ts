import * as C from "./update-all";
import { createResolverMocks } from "@/commands/client-resolver-mock";
import { CommandState } from "@/commands/type";
import { Configuration } from "@/generated/configuration_pb";
import { actions } from "@/modules/configuration";

describe("Commands", () => {
  describe("internal:configuration:Update All", () => {
    test("call action to update all configuration at once", () => {
      const mocks = createResolverMocks();
      const command = C.createCommand();
      const config = new Configuration();
      config.setKeyList(["a", "b", "c"]);
      config.setJsonValue(JSON.stringify("false"));

      command.execute(mocks.dispatcher, {} as CommandState, {
        configurations: [config],
      });

      expect(mocks.dispatcher.dispatch).toBeCalledWith(actions.update([config]));
    });
  });
});
