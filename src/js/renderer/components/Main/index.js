import React from 'react';
import ReactDOM from 'react-dom';

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

    KeyHandler.handleKeyEvents(e.key, this.state);
  }
}
