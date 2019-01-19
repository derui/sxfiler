import * as kbd from "./index";

it("can make key with empty option", () => {
  const key = kbd.make("key");

  expect(key.key).toEqual("key");
  expect(key.meta).toEqual(false);
  expect(key.ctrl).toEqual(false);
});

describe("make key with options", () => {
  it("can make key with single option for meta", () => {
    const key = kbd.make("key", { meta: true });

    expect(key.meta).toEqual(true);
    expect(key.ctrl).toEqual(false);
  });

  it("can make key with single option for ctrl", () => {
    const key = kbd.make("key", { ctrl: true });

    expect(key.meta).toEqual(false);
    expect(key.ctrl).toEqual(true);
  });

  it("can make key with all options", () => {
    const key = kbd.make("key", { ctrl: true, meta: true });

    expect(key.meta).toEqual(true);
    expect(key.ctrl).toEqual(true);
  });
});

describe("convert to key sequence", () => {
  it("can convert object having only key", () => {
    const key = kbd.make("key");

    expect(kbd.toKeySeq(key)).toEqual("key");
  });

  it("can convert object having meta modifier", () => {
    const key = kbd.make("key", { meta: true });

    expect(kbd.toKeySeq(key)).toEqual("M-key");
  });

  it("can convert object having ctrl modifier", () => {
    const key = kbd.make("key", { ctrl: true });

    expect(kbd.toKeySeq(key)).toEqual("C-key");
  });

  it("can convert object having all modifiero", () => {
    const key = kbd.make("key", { ctrl: true, meta: true });

    expect(kbd.toKeySeq(key)).toEqual("M-C-key");
  });
});
