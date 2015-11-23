/// <reference path="../../../../../typings/tsd.d.ts" />
import * as React from 'react';
import R from 'ramda';
import tmpl from './index-rt.rt.js';
import {PaneInfo, GlobalState} from 'sxfiler/renderer/types';
import {ActionStream} from 'sxfiler/renderer/action-stream';

import {PANE} from 'sxfiler/common/constants';

interface Prop {
  state: GlobalState;
  actionStream: ActionStream;
}

interface State {
  leftSelected: boolean;
  rightSelected: boolean;
  left: PaneInfo;
  right: PaneInfo;
}

/**
 * SxFiler Entry point.
 */
export default class Main extends React.Component<Prop, State> {

  /**
   * Construct a App
   *
   * @param {object} props Properties for this component.
   */
  constructor(props: Prop) {
    super(props);
    let currentPane = props.state.currentPane;

    this.state = {
      leftSelected: currentPane === PANE.LEFT,
      rightSelected: currentPane === PANE.RIGHT,
      left: props.state.paneInfo[PANE.LEFT],
      right: props.state.paneInfo[PANE.RIGHT]
    };
  }

  public componentWillReceiveProps(newProps: Prop): void {
    let currentPane = newProps.state.currentPane;
    this.setState({
      leftSelected: currentPane === PANE.LEFT,
      rightSelected: currentPane === PANE.RIGHT,
      left: newProps.state.paneInfo[PANE.LEFT],
      right: newProps.state.paneInfo[PANE.RIGHT]
    });
  }

  /**
   * @override
   *
   * @return {React.Element}
   */
  public render(): JSX.Element {
    return tmpl.apply(this);
  }
}
