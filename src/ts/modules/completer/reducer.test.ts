import { reducer } from "./reducer";
import { pipe } from "@/libs/fn";
import { actions } from "./actions";
import { Candidate } from "@/generated/completer_pb";

describe("Modules", () => {
  describe("Completer", () => {
    describe("Reducer", () => {
      test("move forward current cursor position when it do not reach end of candidates", () => {
        const candidates = [new Candidate(), new Candidate(), new Candidate()];
        const state = pipe(
          (v) => reducer(v, actions.updateCandidates(candidates)),
          (v) => reducer(v, actions.cursorDown())
        )(undefined);

        expect(state.currentCursorPosition.value).toEqual(1);
      });

      test("move backword current cursor position when it do not readh beginning of candidates", () => {
        const candidates = [new Candidate(), new Candidate(), new Candidate()];
        let state = pipe(
          (v) => reducer(v, actions.updateCandidates(candidates)),
          (v) => reducer(v, actions.cursorDown())
        )(undefined);

        expect(state.currentCursorPosition.value).toEqual(1);

        state = reducer(state, actions.cursorUp());

        expect(state.currentCursorPosition.value).toEqual(0);
      });

      test("do not move backword when cursor already reached to beginning of candidates", () => {
        const state = reducer(undefined, actions.cursorUp());

        expect(state.currentCursorPosition.value).toEqual(0);
      });

      test("do not move forward when cursor already reached to end of candidates", () => {
        const state = pipe(
          (v) => reducer(v, actions.updateCandidates([new Candidate(), new Candidate()])),
          (v) => reducer(v, actions.cursorDown()),
          (v) => reducer(v, actions.cursorDown())
        )(undefined);

        expect(state.currentCursorPosition.value).toEqual(1);
      });

      test("open completer when open action given", () => {
        const state = reducer(undefined, actions.open("title"));

        expect(state.opened).toBeTruthy();
      });

      test("close completer when close action given", () => {
        let state = reducer(undefined, actions.open("title"));

        expect(state.opened).toBeTruthy();

        state = reducer(state, actions.close());
        expect(state.opened).toBeFalsy();
      });
    });
  });
});
