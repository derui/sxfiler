import test from 'tape';
import Bus from 'sxfiler/renderer/utils/Bus';

/** @test {Bus} */
test("renderer/utils/Bus", (t) => {
  t.test("should be able to get named bus", (st) => {
    let bus = new Bus();
    let channel = bus.bus("name");

    st.notEqual(channel, null);
    st.equal(bus.bus("name"), channel);
    st.end();
  });

  t.test("can push any value to named channel", (st) => {
    let bus = new Bus();
    let channel = bus.bus("channel");
    channel.subscribe(([v]) => {
      st.equal(v, 100);
      st.end();
    });

    bus.push('channel', 100);
  });

  t.end();
});
