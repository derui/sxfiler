import { Configuration } from "@/domains/configuration";

export interface State {
  // current configuration
  current: Configuration;
}

/** return empty state */
export function empty(): State {
  return { current: new Configuration() };
}
