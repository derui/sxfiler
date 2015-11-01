import React from 'react';
import ReactDOM from 'react-dom';

import {Pane} from 'sxfiler/common/Constants';
import tmpl from './indexRT.rt';
import KeyHandler from './KeyHandler';

/**
 * SxFiler Entry point.
 */
export default class App extends React.Component {
  /**
   * Construct a App
   *
   * @param {object} props Properties for this component.
   */
  constructor(props) {
    super(props);

    this.state = {
      leftSelected: props.paneInfo.current === Pane.LEFT,
      rightSelected: props.paneInfo.current === Pane.RIGHT
    };
  }

  componentWillReceiveProps(newProps) {
    this.state = {
      leftSelected: newProps.paneInfo.current === Pane.LEFT,
      rightSelected: newProps.paneInfo.current === Pane.RIGHT
    };
  }

  /**
   * @override
   */
  componentDidMount() {
    ReactDOM.findDOMNode(this).focus();
  }

  /**
   * @override
   *
   * @return {React.Element}
   */
  render() {
    return tmpl.apply(this);
  }

  /**
   * Handler for keydown/keypress event on this component.
   * @param {Event} e event object
   */
  handleKeyEvents(e) {
    e.stopPropagation();

    let shouldPrevent = KeyHandler.handleKeyEvents(e.key, this.props);
    if (shouldPrevent) {
      e.preventDefault();
    }
  }
}
