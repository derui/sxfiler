---
to: src/ts/commands/<%= group %>/<%= module %>/<%= name %>.test.ts
---
import * as C from "./<%= name %>";

describe("Commands", () => {
  describe("<%= group %>:<%= module %>:<%= h.changeCase.title(name) %>", () => {
  });
});
