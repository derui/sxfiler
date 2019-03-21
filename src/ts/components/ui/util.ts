import * as React from "react";

// simple function to apply component display name
export function applyDisplayName<T>(name: string, component: React.ComponentType<T>) {
  component.displayName = name;
  return component;
}

// Helper type-level function to extract complex proptype automatically
export type ExtractProps<T extends React.ComponentType<any>> = T extends React.ComponentType<infer P> ? P : never;

// The type of forwardedRef that is used with RootRef
export type ForwardedRef<P extends HTMLElement = HTMLElement> = {
  forwardedRef?: React.Ref<P>;
};
