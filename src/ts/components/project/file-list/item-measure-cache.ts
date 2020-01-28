export type Measure = {
  width: number;
  height: number;
};

type Cache = Map<number, Measure>;
type ReadonlyCache = ReadonlyMap<number, Measure>;
type Handler = (cache: ReadonlyCache) => void;

export class ItemMeasureCache {
  private cache: Cache = new Map();
  private handlers: Handler[] = [];

  // regist handler to observe cache changes.
  public observe(handler: Handler): () => void {
    this.handlers.push(handler);
    return () => {
      this.handlers = this.handlers.filter((v) => v !== handler);
    };
  }

  public set(key: number, e: HTMLElement | null) {
    if (!e) {
      return;
    }

    const measure = {
      width: e.clientWidth,
      height: e.clientHeight,
    };

    this.cache.set(key, measure);

    const updatedCache = new Map(this.cache);
    this.handlers.forEach((v) => v(updatedCache));
  }

  public get(key: number) {
    return this.cache.get(key);
  }

  public clear() {
    this.cache.clear();
  }
}
