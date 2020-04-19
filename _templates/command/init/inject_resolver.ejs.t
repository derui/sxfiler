---
to: src/ts/commands/index.ts
inject: true
skip_if: <%= h.changeCase.camel(name) %>\.registerToResolver
after: \/\/#COMMAND REGISTER
---
  <%= h.changeCase.camel(name) %>.registerToResolver(resolver);