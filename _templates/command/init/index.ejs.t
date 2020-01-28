---
to: src/ts/commands/<%= name %>/index.ts
---
import { DescriptorsType } from "@/commands/type";
import { Type } from "../command-resolver";

// prettier-ignore
export const descriptors = {
} as const;

export type Descriptors = DescriptorsType<typeof descriptors>;

/**
 * register commands
 */
// prettier-ignore
export const registerToResolver = (resolver: Type) => {
};