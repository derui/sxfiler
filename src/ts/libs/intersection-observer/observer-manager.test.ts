import * as observer from "./observer-manager";

afterEach(() => {
  jest.restoreAllMocks();
});

describe("Observer Manager", () => {
  it("create and delete root", () => {
    const id = observer.createRoot();

    expect(observer.deleteRoot(id)).toBeTruthy();
  });

  it("should be able to observe and unobserve the target before register target", () => {
    const id = observer.createRoot();

    const target = {
      element: document.createElement("span"),
      handler: () => {},
    };
    observer.observe(id, target);

    expect(observer.unobserve(target)).toEqual(true);
  });

  it("should unobserve from all observer", () => {
    const id = observer.createRoot();
    const id2 = observer.createRoot();

    const target = {
      element: document.createElement("span"),
      handler: () => {},
    };
    observer.observe(id, target);
    observer.observe(id2, target);

    expect(observer.unobserve(target)).toEqual(true);
    expect(observer.observingTargets(id)).toHaveLength(0);
    expect(observer.observingTargets(id2)).toHaveLength(0);
  });

  it("should throw error if root is not found", () => {
    const target = {
      element: document.createElement("span"),
      handler: () => {},
    };

    expect(() => observer.observe("id", target)).toThrow();
  });

  it("register observer root with option", () => {
    const original = IntersectionObserver;
    const mockInstance = jest.fn();
    IntersectionObserver = mockInstance;

    const id = observer.createRoot();
    const option = {
      root: document.createElement("span"),
    };
    observer.registerObserverRoot(id, option);

    expect(mockInstance.mock.instances[0]).not.toBeNull();

    IntersectionObserver = original;
  });

  it("throws if rootMargin is invalid", () => {
    const id = observer.createRoot();
    const option = {
      root: document.createElement("span"),
      rootMargin: "10",
    };
    expect(() => observer.registerObserverRoot(id, option)).toThrowErrorMatchingSnapshot();
  });

  it("unregister existance instance if register new root ", () => {
    const id = observer.createRoot();
    const option = {
      root: document.createElement("span"),
    };
    const firstObserver = observer.registerObserverRoot(id, option);
    const spy = jest.spyOn(firstObserver, "disconnect");
    const secondObserver = observer.registerObserverRoot(id, {
      root: document.createElement("div"),
    });

    expect(spy.mock.calls).toHaveLength(1);
    expect(firstObserver).not.toStrictEqual(secondObserver);
  });

  it("unregister observing element if register new root ", () => {
    const id = observer.createRoot();
    const target = {
      element: document.createElement("a"),
      handler: () => {},
    };
    const option = {
      root: document.createElement("span"),
    };
    const firstObserver = observer.registerObserverRoot(id, option);
    observer.observe(id, target);

    const spy = jest.spyOn(firstObserver, "disconnect");
    const spyUnobserve = jest.spyOn(firstObserver, "unobserve");
    const secondObserver = observer.registerObserverRoot(id, {
      root: document.createElement("div"),
    });

    expect(spy.mock.calls).toHaveLength(1);
    expect(spyUnobserve.mock.calls[0][0]).toEqual(target.element);
    expect(firstObserver).not.toStrictEqual(secondObserver);
  });
});
