import Factory from "./filer-factory";

describe("Factory for filer", () => {
  it("can create filer", () => {
    const filer = Factory.create({ id: "id", location: "/foo", nodes: [] });

    expect(filer.id).toEqual("id");
    expect(filer.location).toEqual("/foo");
    expect(filer.currentCursorIndex).toEqual(0);
  });
});
