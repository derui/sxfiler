// reducer for config
import { Actions } from "../actions";
import { empty, State } from "../types/store-state/config";

export function reducer(state: State = empty(), action: Actions): State {
  switch (action.type) {
  }
  return state;
}

export default reducer;
