---
to: src/ts/commands/<%= group %>/index.ts
inject: true
skip_if: resolver.register\(\{descriptor. <%= h.changeCase.camel(`${module}-${name}`) %>
after: export const registerToResolver =
---
<%_ const camelName = h.changeCase.camel(`${module}-${name}`) _%>
  resolver.register({descriptor: <%= camelName %>.descriptor, factory: <%= camelName %>.createCommand});