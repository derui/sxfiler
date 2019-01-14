/**
   implement for TypeScript version sxfiler_kbd.
 */

interface Key {
  ctrl: boolean;
  meta: boolean;
  key: string;
};

type MakeOption = {
  ctrl?: boolean;
  meta?: boolean;
}

/**
 * make key type
 * @param key A key that input from keyborad event
 * @param option option to make key
 */
export function make(key: string, option: MakeOption = {ctrl: false, meta: false}): Key {
  return {
    key,
    ctrl: option.ctrl || false,
    meta: option.meta || false,
  };
}

/**
 * convert Key interface to key sequence string representation that is compatible of sxfiler_kbd's.
 * @param key
 */
export function toKeySeq(key: Key): string {
  const meta = key.meta ? "M-" : "";
  const ctrl = key.ctrl ? "C-" : "";

  return `${meta}${ctrl}${key.key}`;
}
