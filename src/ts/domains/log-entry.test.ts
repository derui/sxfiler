import { createLogEntry, Level } from "./log-entry";

describe("Log Entry Domain", () => {
  it("create log entry by factory", () => {
    const data = createLogEntry({
      id: "log1",
      body: "message",
      level: Level.Error,
    });

    expect(data.id).toEqual("log1");
    expect(data.body).toEqual("message");
    expect(data.level).toEqual(Level.Error);
  });
});
