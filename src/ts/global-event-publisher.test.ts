import * as EventHub from "./typed-event-hub";
import { create } from "./global-event-publisher";
import { reducer, actions, emptyState } from "./modules/decision";
import * as Root from "./modules";
import { DecisionRequiredOp, DecisionAction } from "./modules/decision/reducer";
import { FileItem } from "./generated/filer_pb";

describe("Global event publisher", () => {
  test("publish event when DecisionFinished action given", (done) => {
    const eventHub = EventHub.create();
    eventHub.once(EventHub.EventTypes.FinishDecision, (e) => {
      expect(e).toEqual(
        EventHub.EventCreators.finishDecision({
          decisionOp: DecisionRequiredOp.Copy,
          processId: "foo",
          resultAction: { kind: DecisionAction.Overwrite },
        })
      );
      done();
    });

    const middleware = create(eventHub);
    const next = jest.fn();
    const getState = jest.fn().mockImplementationOnce(() => {
      return {
        ...Root.emptyState,
        decision: reducer(emptyState, actions.requireDecisionForCopy("foo", new FileItem())),
      } as Root.State;
    });
    const action = actions.finish();

    middleware({ getState } as any)(next)(action);
  });
});
