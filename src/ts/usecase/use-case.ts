// define interface of UseCase

export default interface UseCase<Param, Result> {
  /**
   * execute usecase with parameter.
   * @param param parameter object
   */
  execute(param: Param): Promise<Result>;
}
