import { createContext } from "./context";
import { CommandLike, CommandState } from "@/commands/type";

describe("Context", () => {
  let client = {
    use: jest.fn(),
  };
  let dispatcher = {
    dispatch: jest.fn(),
  };
  let useCase: CommandLike<{ foo: string }> = {
    execute: jest.fn(),
    identifier: "test",
  };

  afterEach(() => {
    jest.clearAllMocks();
  });

  it("make executor for us case execution", () => {
    const context = createContext({
      dispatcher,
    });
    const arg = { foo: "bar" };
    const state = {} as CommandState;

    context.use(useCase)(state, arg);

    expect(useCase.execute).toBeCalledWith(dispatcher, state, arg);
  });
});
