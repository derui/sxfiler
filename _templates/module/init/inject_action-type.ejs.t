---
to: src/ts/modules/index.ts
inject: true
skip_if: import.+<%= name %>
after: export type Actions =
---
  | <%= h.changeCase.pascal(name) %>.Actions