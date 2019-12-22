import * as E from "./bookmark";
import { Bookmark } from "@/generated/bookmark_pb";
import { createBookmark } from "@/domains/bookmark";

describe("Object Codecs", () => {
  describe("Candidate", () => {
    it("convert JSON representation to frontend domain", () => {
      const bookmark = new Bookmark();
      bookmark.setId("id");
      bookmark.setPath("/path");
      bookmark.setOrder(1);
      const obj = E.encode(bookmark);

      expect(obj).toEqual(createBookmark({ id: "id", path: "/path", order: 1 }));
    });
  });
});
