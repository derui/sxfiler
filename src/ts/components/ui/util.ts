import * as React from "react";

// Helper type-level function to extract complex proptype automatically
export type ExtractProps<T extends React.ComponentType<any>> = T extends React.ComponentType<infer P> ? P : never;

/**
 * TとPを合成した型を返す。TとPに同名のキーが存在した場合はPのものが優先される。
 */
export type Extend<T, P> = Pick<T, Exclude<keyof T, keyof P>> & P;
