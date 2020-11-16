import * as C from "./apply-file-list-event";
import * as M from "@/commands/client-resolver-mock";
import { State } from "@/modules";
import { actions } from "@/modules/filer";
import { FileList, FileListEventType } from "@/generated/filer_pb";

describe("Commands", () => {
  describe("internal:filer:Apply File List Event", () => {
    test("dispatch action to update file list", async () => {
      const mocks = M.createResolverMocks();
      const command = C.createCommand();
      const state = {} as State;
      const fileList = new FileList();

      await command.execute(
        mocks.dispatcher,
        { clientResolver: mocks.clientResolver, state },
        {
          fileListEventType: FileListEventType.LOCATION_CHANGED,
          fileList: fileList,
        }
      );

      expect(mocks.dispatcher.dispatch).toBeCalledWith(
        actions.applyFileListEvent(FileListEventType.LOCATION_CHANGED, fileList)
      );
    });
  });
});
