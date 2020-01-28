---
to: src/ts/modules/<%= name %>/reducer.test.ts
---
import {reducer} from "./reducer";

describe("Modules", () => {
  describe("<%= h.changeCase.title(name) %>", () => {
    describe("Reducer", () => {
    });
  });
});