import * as C from "./select-section";
import { createResolverMocks } from "@/commands/client-resolver-mock";
import { CommandState } from "@/commands/type";
import { createSection } from "@/configurations/creators";
import { actions } from "@/modules/configuration";

describe("Commands", () => {
  describe("internal:configuration:Select Section", () => {
    test("dispatch action to select section", () => {
      const mocks = createResolverMocks();
      const command = C.createCommand();
      const section = createSection({
        key: ["category", "section"],
        displayName: "section",
        description: "desc",
        items: [],
      });

      command.execute(mocks.dispatcher, {} as CommandState, {
        section,
      });

      expect(mocks.dispatcher.dispatch).toBeCalledWith(actions.selectSection(section));
    });
  });
});
