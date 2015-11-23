/// <reference path="../../../../../typings/tsd.d.ts" />
import _ from 'lodash';
import Util from 'sxfiler/renderer/utils/file-type-util';

/**
 * Permission number to posix-style permission string.
 * 
 * @param {string} perm Permission number to convert string
 * @return {string} Converted string from permission
 */
function permissionToString(perm: string): string {
  'use strict';
  let num = parseInt(perm, 10);
  let execute = (num & 0x1) ? 'x' : '-';
  let write = (num & 0x2) ? 'w' : '-';
  let read = (num & 0x4) ? 'r' : '-';
  return `${read}${write}${execute}`;
}

/**
 * Get posix style permission string.
 * 
 * @param {number} mode A permission number of the file
 * @return {string} posix-style mode string (Read-Write-eXcutable)
 */
export default function getModeString(mode: number): string {
  'use strict';
  if (isNaN(mode) || !_.isNumber(mode)) {
    return '----------';
  }

  let permission = `${(mode & 0o777).toString(8)}`;
  const padding = '000';
  permission = padding.substring(0, padding.length - permission.length) + permission;
  let other = permissionToString(permission.slice(2, 3));
  let group = permissionToString(permission.slice(1, 2));
  let owner = permissionToString(permission.slice(0, 1));

  return `${Util.modeToFileType(mode)}${owner}${group}${other}`;
}
