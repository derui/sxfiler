/**
   Type of bookmark
 */
export type Bookmark = {
  readonly id: string;
  readonly path: string;
  readonly order: number;
};

type FactoryArg = Pick<Bookmark, keyof Bookmark>;

/**
 * Create bookmark from factory argument
 */
export const createBookmark = function createBookmark({ id, path, order }: FactoryArg): Bookmark {
  return {
    id,
    path,
    order,
  };
};
