import { Actions } from "./actions";
import { createContext } from "./context";
import { UseCaseLike } from "./usecases/type";

describe("Context", () => {
  let client = {
    call: jest.fn(),
    notify: jest.fn(),
  };
  let dispatcher = {
    dispatch: jest.fn(),
  };
  let useCase: UseCaseLike<Actions, { foo: string }> = {
    execute: jest.fn(),
  };

  afterEach(() => {
    jest.clearAllMocks();
  });

  it("make executor for us case execution", () => {
    const context = createContext({
      client,
      dispatcher,
    });
    const arg = { foo: "bar" };

    context.use(useCase)(arg);

    expect(useCase.execute).toBeCalledWith(dispatcher, arg);
  });
});
