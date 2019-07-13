import * as React from "react";
import { mount } from "enzyme";
import * as Modal from "./completer";
import * as ListItem from "@/components/ui/list-item/list-item";
import { createCandidate } from "@/domains/candidate";

describe("Project", () => {
  describe("Completer", () => {
    it("render correctly", () => {
      const root = document.createElement("div");
      const tree = mount(
        <Modal.Component
          dialogRoot={root}
          opened={true}
          container={{
            title: "test",
            selectedItemIndex: -1,
            items: [],
            onInput: () => {},
          }}
        />
      );

      expect(tree.find("input")).toHaveLength(1);
    });

    it("render with a candidate", () => {
      const root = document.createElement("div");
      const tree = mount(
        <Modal.Component
          dialogRoot={root}
          opened={true}
          container={{
            title: "test",
            selectedItemIndex: -1,
            items: [createCandidate({ id: "id", value: "value", start: 0, length: 0 })],
            onInput: () => {},
          }}
        />
      );

      expect(tree.find(ListItem.Component)).toHaveLength(1);
    });

    it("render with candidates", () => {
      const root = document.createElement("div");
      const tree = mount(
        <Modal.Component
          dialogRoot={root}
          opened={true}
          container={{
            title: "test",
            selectedItemIndex: -1,
            items: [
              createCandidate({ id: "id", value: "value", start: 0, length: 0 }),
              createCandidate({ id: "id2", value: "value2", start: 0, length: 0 }),
            ],
            onInput: () => {},
          }}
        />
      );

      expect(tree.find(ListItem.Component)).toHaveLength(2);
    });

    it("call callback when change input", () => {
      const root = document.createElement("div");
      const promise = new Promise(resolve => {
        const tree = mount(
          <Modal.Component
            dialogRoot={root}
            opened={true}
            container={{
              title: "test",
              selectedItemIndex: -1,
              items: [
                createCandidate({ id: "id", value: "value", start: 0, length: 0 }),
                createCandidate({ id: "id2", value: "value2", start: 0, length: 0 }),
              ],
              onInput: v => {
                expect(v).toEqual("foo");
                resolve();
              },
            }}
          />
        );

        tree.find('input[type="text"]').simulate("change", {
          target: {
            value: "foo",
          },
        });
      });

      return promise;
    });
  });
});
