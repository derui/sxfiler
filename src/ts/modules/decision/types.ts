// prettier-ignore
export const ActionTypes = {
RESET: "decision/reset",
UPDATE_NEW_NAME: "decision/updateNewName",
SELECT_PREVIOUS_ACTION: "decision/selectPreviousAction",
SELECT_NEXT_ACTION: "decision/selectNextAction",
FINISH: "decision/finish",
REQUIRE_DECISION_FOR_MOVE: "decision/requireDecisionForMove",
REQUIRE_DECISION_FOR_DELETE: "decision/requireDecisionForDelete",
REQUIRE_DECISION_FOR_COPY: "decision/requireDecisionForCopy",
} as const;
