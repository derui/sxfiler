// define application-global intersection observer management.
import uuid from "uuid";
import "intersection-observer";

export type RootId = string;
export type IntersectionChandedHandler = (entry: IntersectionObserverEntry) => void;

export interface IntersectionTarget {
  element: Element;
  handler: IntersectionChandedHandler;
}

interface Observer {
  instance: IntersectionObserver | undefined;
  targets: Map<Element, IntersectionTarget>;
}

const intersectionObserverMap = new Map<RootId, Observer>();

// get observer binded rootId
function getObserver(rootId: RootId): Observer | undefined {
  return intersectionObserverMap.get(rootId);
}

export function observingTargets(rootId: RootId): IntersectionTarget[] | undefined {
  const observer = intersectionObserverMap.get(rootId);

  if (!observer) {
    return undefined;
  }

  return Array.from(observer.targets.values());
}

// observe the element by root
export function observe(rootId: RootId, target: IntersectionTarget) {
  const observer = getObserver(rootId);

  if (!observer) {
    throw new Error(`Not found root instance by ${rootId}, try "createRoot" first.`);
  }

  observer.targets.set(target.element, target);

  const { instance } = observer;
  if (instance) {
    instance.observe(target.element);
  }
}

// unobserve the element from root
export function unobserve(target: IntersectionTarget): boolean {
  let deleted = false;

  for (let entry of intersectionObserverMap.entries()) {
    let [_, observer] = entry;
    if (observer.instance) {
      observer.instance.unobserve(target.element);
    }
    deleted = observer.targets.delete(target.element) || deleted;
  }

  return deleted;
}

// create and register new root
export function createRoot(): RootId {
  const id = uuid.v4();

  intersectionObserverMap.set(id, { instance: undefined, targets: new Map() });

  return id;
}

export function deleteRoot(rootId: RootId): boolean {
  const observer = getObserver(rootId);

  if (!observer) {
    return false;
  }

  intersectionObserverMap.delete(rootId);

  const { instance, targets } = observer;
  if (instance) {
    targets.forEach(e => instance.unobserve(e.element));
    instance.disconnect();
  }

  return true;
}

function callback(rootId: RootId): (entries: IntersectionObserverEntry[]) => void {
  return entries => {
    const observer = getObserver(rootId);

    if (!observer) {
      throw new Error(`Not found root instance of ${rootId}`);
    }

    entries.forEach(entry => {
      const target = observer.targets.get(entry.target);
      if (target) {
        target.handler(entry);
      }
    });
  };
}

export function registerObserverRoot(rootId: RootId, option: IntersectionObserverInit): IntersectionObserver {
  const observer = getObserver(rootId);

  if (!observer) {
    throw new Error(`Not found root ${rootId}`);
  }

  const { instance, targets } = observer;
  if (observer && instance && instance.root !== option.root) {
    targets.forEach(e => instance.unobserve(e.element));
    instance.disconnect();
    observer.instance = undefined;
  }

  const newInstance = new IntersectionObserver(callback(rootId), option);
  observer.instance = newInstance;
  targets.forEach(e => newInstance.observe(e.element));
  return newInstance;
}
