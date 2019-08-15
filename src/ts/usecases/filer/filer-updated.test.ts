import * as actions from "@/actions/filer";
import * as U from "./filer-updated";
import { createFiler } from "@/domains/filer";
import { createLocationHistory } from "@/domains/location-history";

describe("UseCase", () => {
  describe("Filer", () => {
    describe("Filer Updated", () => {
      it("execute action to load filer from server state", () => {
        const useCase = U.createUseCase();
        const dispatcher = {
          dispatch: jest.fn(),
        };

        const filer = createFiler({
          id: "id",
          name: "name",
          location: "/",
          items: [],
          currentCursorIndex: 0,
          history: createLocationHistory({ records: [], maxRecordNumber: 0 }),
        });

        useCase.execute(dispatcher, { filer });
        expect(dispatcher.dispatch).toBeCalledWith(actions.load({ filer }));
      });
    });
  });
});
