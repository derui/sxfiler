import assert from 'power-assert';
import Bus from 'sxfiler/renderer/utils/Bus';

/** @test {Bus} */
describe("renderer/utils/Bus", () => {
  it("should be able to get named bus", () => {
    let bus = new Bus();
    let channel = bus.bus("name");

    assert.notEqual(channel, null);
    assert.equal(bus.bus("name"), channel);
  });

  it("can push any value to named channel", (cb) => {
    let bus = new Bus();
    let channel = bus.bus("channel");
    channel.subscribe((v) => {
      assert.equal(v, 100);
      cb();
    });

    bus.push('channel', 100);
  });
});
