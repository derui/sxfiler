---
to: src/ts/modules/<%= name %>/selectors.test.ts
---
import * as S from "./selectors";

describe("Modules", () => {
  describe("<%= h.changeCase.title(name) %>", () => {
    describe("Selectors", () => {
    });
  });
});