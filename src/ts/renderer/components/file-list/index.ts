/// <reference path="../../../../../typings/tsd.d.ts" />
import * as React from 'react';
import tmpl from './index-rt.rt.js';
import * as types from 'sxfiler/common/types';
import * as gs from 'sxfiler/renderer/types';

interface Prop {
  state: gs.GlobalState;
  paneInfo: gs.PaneInfo;
  selected: boolean;
}

interface State {
  position: number;
  items: types.File[];
}

function createState(props: Prop): State {
  'use strict';
  let len = props.paneInfo.fileList.length;

  return {
    items: props.paneInfo.fileList || [],
    position: Math.max(0, Math.min(len - 1, props.paneInfo.selected))
  };
}

/**
 * A list for files and directries.
 *
 * This list only views informations of files and directories in props.
 */
export default class FileList extends React.Component<Prop, State> {
  public static defaultProps: Prop = {
    state: <gs.GlobalState>{},
    paneInfo: <gs.PaneInfo>{},
    selected: false
  };

  /**
   * Construct FileList component.
   *
   * @param {object} props - properties of component
   */
  constructor(props: Prop) {
    super(props);

    this.state = createState(props);
  }

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
