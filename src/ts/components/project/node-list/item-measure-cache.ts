export type Measure = {
  width: number;
  height: number;
};

export class ItemMeasureCache {
  private cache: Map<number, Measure> = new Map();

  public set(key: number, e: HTMLElement | null) {
    if (!e) {
      return;
    }

    const measure = {
      width: e.clientWidth,
      height: e.clientHeight,
    };

    this.cache.set(key, measure);
  }

  public get(key: number) {
    return this.cache.get(key);
  }

  public clear() {
    this.cache.clear();
  }
}
