---
to: src/ts/modules/<%= name %>/types.ts
inject: true
skip_if: <%= h.changeCase.camel(action) %>
after: export const ActionTypes = {
---
<%= h.changeCase.upper(h.changeCase.snake(action)) %>: "<%= name %>/<%= h.changeCase.camel(action) %>",