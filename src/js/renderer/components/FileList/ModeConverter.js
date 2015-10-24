import _ from 'lodash';
import Util from './FileTypeUtil';

/**
 * Permission number to posix-style permission string.
 * 
 * @param {string} perm Permission number to convert string
 * @return {string} Converted string from permission
 */
function permissionToString(perm) {
  let number = parseInt(perm, 10);
  let execute = (perm & 0x1) ? 'x' : '-';
  let write = (perm & 0x2) ? 'w' : '-';
  let read = (perm & 0x4) ? 'r' : '-';
  return `${read}${write}${execute}`;
}

/**
 * Get posix style permission string.
 * 
 * @param {number} mode A permission number of the file
 * @return {string} posix-style mode string (Read-Write-eXcutable)
 */
export default function getModeString(mode) {
  if (isNaN(mode) || !_.isNumber(mode)) {
    return '----------';
  }

  let permission = `${(mode & 0o777).toString(8)}`;
  const padding = '000';
  permission = padding.substring(0, padding.length - permission.length) + permission;
  let other = permissionToString(permission.slice(2,3));
  let group = permissionToString(permission.slice(1,2));
  let owner = permissionToString(permission.slice(0,1));

  return `${Util.modeToFileType(mode)}${owner}${group}${other}`;
}
