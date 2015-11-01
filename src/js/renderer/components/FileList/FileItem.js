import React from 'react';
import tmpl from './FileItemRT.rt';
import getModeString from './ModeConverter';
import Util, {Types} from './FileTypeUtil';

/**
 * Create next state from next props and current state
 * 
 * @param {object} props properties of component
 * @param {object} state previous state of component
 * @return {object} next state
 */
function createState(props, state) {
  let item = props.item || {};
  let stat = item.stat || {};
  let mode = stat.mode || NaN;

  return {
    filename: item.filename || '',
    mode: getModeString(mode),
    size: stat.size || 0,
    type: ((mode) => {
      switch (Util.modeToFileType(mode)) {
      case Types.DIRECTORY:
        return 'directory';
      case Types.SYMLINK:
        return 'symlink';
      default:
        return 'normal';
      }
    })(mode)
  };
}

/**
 * A item of the {FileList}.
 */
export default class FileItem extends React.Component {
  /**
   * Construct FileItem component.
   *
   * @param {object} props - properties of component
   */
  constructor(props) {
    super(props);

    this.state = createState(props, {});
  }

  /**
   * @override
   */
  componentWillReceiveProps(newProps) {
    this.setState(createState(newProps, this.state));
  }

  /**
   * @override
   */
  render() {
    return tmpl.apply(this);
  }
}

FileItem.defaultProps = {
  item: {
    filename: '',
    stat: {}
  }
};
