// value object for configuration of application

enum SortType {
  Name,
  Date,
  Timestamp,
}

export class Configuration {
  public readonly defaultSortOrder: SortType = SortType.Name;
}
