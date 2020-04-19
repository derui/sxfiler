import * as C from "./request";
import * as M from "@/commands/client-resolver-mock";
import { emptyState } from "@/modules";
import { DecisionRequiredOp } from "@/modules/decision/reducer";
import { FileItem } from "@/generated/filer_pb";
import { actions } from "@/modules/decision";

describe("Commands", () => {
  describe("internal:decision:Request", () => {
    test("dispatch copy requester when given operation is copy", async () => {
      const command = C.createCommand();
      const mocks = M.createResolverMocks();
      const fileItem = new FileItem();

      await command.execute(
        mocks.dispatcher,
        { clientResolver: mocks.clientResolver, state: emptyState },
        {
          requiredOp: DecisionRequiredOp.Copy,
          fileItem,
        }
      );

      expect(mocks.dispatcher.dispatch).toBeCalledWith(actions.requireDecisionForCopy("copy", fileItem));
    });

    test("dispatch move requester action when given operation is move", async () => {
      const command = C.createCommand();
      const mocks = M.createResolverMocks();
      const fileItem = new FileItem();

      await command.execute(
        mocks.dispatcher,
        { clientResolver: mocks.clientResolver, state: emptyState },
        {
          requiredOp: DecisionRequiredOp.Move,
          fileItem,
        }
      );

      expect(mocks.dispatcher.dispatch).toBeCalledWith(actions.requireDecisionForMove("move", fileItem));
    });

    test("dispatch delete requester action when given operation is delete", async () => {
      const command = C.createCommand();
      const mocks = M.createResolverMocks();
      const fileItem = new FileItem();

      await command.execute(
        mocks.dispatcher,
        { clientResolver: mocks.clientResolver, state: emptyState },
        {
          requiredOp: DecisionRequiredOp.Delete,
          fileItem,
        }
      );

      expect(mocks.dispatcher.dispatch).toBeCalledWith(actions.requireDecisionForDelete("delete", fileItem));
    });
  });
});
