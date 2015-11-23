import test from 'tape';
import * as maybe from 'sxfiler/common/maybe';

const wrapper = (label, f) => test(`common/maybe ${label}`, f);

wrapper('should equal NONE', t => {
  t.equal(maybe.NONE, maybe.NONE);

  t.end();
});

wrapper('should mapping maybe value with function', t => {
  let m = maybe.just(true);
  m = maybe.fmap(v => !v)(m);

  t.equal(m.val, false);

  t.end();
});

wrapper('should mapping maybe value with function', t => {
  let m = maybe.just(true);
  m = maybe.fmap(v => !v)(m);

  t.equal(maybe.isNone(m), false);

  t.end();
});

wrapper('should return lifted up maybe from multiple wrapped up', t => {
  let m = maybe.just(maybe.just(1));
  m = maybe.flatten(m);
  t.equal(maybe.isSome(m), true);
  t.equal(m.val, 1);

  t.end();
});

wrapper('should always return NONE if fmap or flatten give NONE', t => {
  t.equal(maybe.fmap(x => x)(maybe.NONE), maybe.NONE);
  t.equal(maybe.flatten(maybe.NONE), maybe.NONE);

  t.end();
});

