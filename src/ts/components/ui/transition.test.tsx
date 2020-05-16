import { h } from "preact";
import { render, cleanup, act } from "@testing-library/preact";
import { Transition } from "./transition";

describe("UI Kit", () => {
  describe("Transition", () => {
    beforeEach(() => {
      jest.useFakeTimers();
    });

    afterEach(cleanup);

    test("transition entering to entered when transitioning", async () => {
      let stateRef = null;
      render(
        <Transition in={true} timeout={100}>
          {(state) => {
            stateRef = state;
            return <span data-testid="span">foo</span>;
          }}
        </Transition>
      );

      expect(stateRef).toEqual("entering");

      act(() => {
        jest.advanceTimersByTime(101);
      });

      // wait re-rendering

      expect(stateRef).toEqual("entered");
    });

    test("transition exiting to exited when transitioning", async () => {
      let stateRef = null;
      const { rerender, findByTestId } = render(
        <Transition in={true} timeout={100}>
          {(state) => {
            stateRef = state;
            return <span data-testid="span">foo</span>;
          }}
        </Transition>
      );

      expect(stateRef).toEqual("entering");

      act(() => {
        jest.advanceTimersByTime(101);
      });
      expect(stateRef).toEqual("entered");

      act(() =>
        rerender(
          <Transition in={false} timeout={100}>
            {(state) => {
              stateRef = state;
              return <span data-testid="span">foo</span>;
            }}
          </Transition>
        )
      );
      expect(stateRef).toEqual("exiting");

      act(() => {
        jest.advanceTimersByTime(101);
      });
      expect(stateRef).toEqual("exited");

      const span = await findByTestId("span");
      expect(span.innerHTML).toContain("foo");
    });

    test("do not transition *ed state if change props in transition", async () => {
      let stateRef = null;
      const { rerender, findByTestId } = render(
        <Transition in={true} timeout={100}>
          {(state) => {
            stateRef = state;
            return <span data-testid="span">foo</span>;
          }}
        </Transition>
      );

      expect(stateRef).toEqual("entering");
      act(() => {
        jest.advanceTimersByTime(50);
      });
      expect(stateRef).toEqual("entering");

      act(() =>
        rerender(
          <Transition in={false} timeout={100}>
            {(state) => {
              stateRef = state;
              return <span data-testid="span">foo</span>;
            }}
          </Transition>
        )
      );
      expect(stateRef).toEqual("exiting");

      act(() => {
        jest.advanceTimersByTime(101);
      });
      expect(stateRef).toEqual("exited");

      const span = await findByTestId("span");
      expect(span.innerHTML).toContain("foo");
    });
  });
});
