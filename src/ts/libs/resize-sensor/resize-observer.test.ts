import * as T from "./resize-observer";

describe("Resize Observer Manager", () => {
  it("can observe and unobserve element and callback", () => {
    const element = document.createElement("div");
    const callback = () => {};

    T.observe(element, callback);
    T.unobserve(element);
  });
});
