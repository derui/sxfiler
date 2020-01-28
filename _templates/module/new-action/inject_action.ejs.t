---
to: src/ts/modules/<%= name %>/actions.ts
inject: true
skip_if: <%= h.changeCase.pascal(action) %>
after: export const actions = {
---
<%= h.changeCase.camel(action) %>,