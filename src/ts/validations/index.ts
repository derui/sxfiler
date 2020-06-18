export namespace Validators {
  const validateIntegerRegexp = /^-?\d+$/;

  // validate a string what it is integer
  export const validateInteger = (value: string | null | undefined): boolean => {
    if (!value) {
      return false;
    }

    return validateIntegerRegexp.test(value);
  };
}
