---
to: src/ts/commands/<%= group %>/index.ts
inject: true
skip_if: <%= h.changeCase.camel(`${module}-${name}`) %>
after: export const descriptors = \{
---
<%_ const camelName = h.changeCase.camel(`${module}-${name}`) _%>
  <%= camelName %>: <%= camelName %>.descriptor,