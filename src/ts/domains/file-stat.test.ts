import bigInt from "big-integer";

import { FileStat } from "./file-stat";
import { Mode } from "./mode";

describe("File Stat", () => {
  it("can get size as bigInt", () => {
    const stat = new FileStat(new Mode(), 1000, 1000, new Date(), new Date(), new Date(), "1000", false, true, false);

    expect(stat.sizeAsBigInt).toEqual(bigInt(1000));
  });
});
