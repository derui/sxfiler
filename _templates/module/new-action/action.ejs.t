---
to: src/ts/modules/<%= name %>/actions.ts
inject: true
skip_if: const <%= h.changeCase.camel(action) %> =
after: \/\/#ACTION INSERTION INDICATOR
---
<%_ const actionName = h.changeCase.camel(action); _%>
export const <%= actionName %> = () => {
  // TODO: Implement please
};