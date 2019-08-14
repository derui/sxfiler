import { Configuration } from "@/domains/configuration";

export interface State {
  // current configuration
  current: Configuration;
}

/** return empty state */
export const empty = function empty(): State {
  return { current: new Configuration() };
};
