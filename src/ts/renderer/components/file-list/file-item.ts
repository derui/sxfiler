import * as React from 'react';
import tmpl from './file-item-rt.rt.js';
import getModeString from './mode-converter';
import Util, {TYPES} from 'sxfiler/renderer/utils/file-type-util';
import * as fs from 'fs';
import * as types from 'sxfiler/common/types';

interface Prop {
  item: types.File;
}

interface State {
  filename: string;
  mode: string;
  size: number;
  fileType: string;
}

/**
 * Create next state from next props and current state
 * 
 * @param {object} props properties of component
 * @return {object} next state
 */
function createState(props: Prop): State {
  'use strict';
  let item: types.File = props.item || <types.File>{};
  let stat: fs.Stats = item.stat || <fs.Stats>{};
  let mode = stat.mode || NaN;

  return {
    fileType: ((mode: number): string => {
      switch (Util.modeToFileType(mode)) {
      case TYPES.DIRECTORY:
        return 'directory';
      case TYPES.SYMLINK:
        return 'symlink';
      default:
        return 'normal';
      }
    })(mode),
    filename: item.filename || '',
    mode: getModeString(mode),
    size: stat.size || 0,
  };
}

/**
 * A item of the {FileList}.
 */
export default class FileItem extends React.Component<Prop, State> {
  public static defaultProps: Prop = {
    item: {
      filename: '',
      id: null,
      stat: <fs.Stats>{}
    }
  };

  /**
   * Construct FileItem component.
   *
   * @param {object} props - properties of component
   */
  constructor(props: Prop) {
    super(props);

    this.state = createState(props);
  }

  /**
   * @override
   */
  public componentWillReceiveProps(newProps: Prop): void {
    this.setState(createState(newProps));
  }

  /**
   * @override
   */
  public render(): JSX.Element {
    return tmpl.apply(this);
  }
}

