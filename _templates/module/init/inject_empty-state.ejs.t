---
to: src/ts/modules/index.ts
inject: true
skip_if: <%= h.changeCase.pascal(name) %>.reducer,
after: export const emptyState =
---
  <%= h.changeCase.camel(name) %>: <%= h.changeCase.pascal(name) %>.emptyState,