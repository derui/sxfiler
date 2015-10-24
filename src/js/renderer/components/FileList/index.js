import React from 'react';
import tmpl from './indexRT.rt';
import R from 'ramda';

function createState(props) {
  let items = props.files;
  let map = R.addIndex(R.map);
  items = R.pipe(R.toPairs, map(([key, value], index) => ({
    id: index,
    filename: key,
    stat: value
  })))(items);

  return {
    current: Math.min(Math.max(props.current, 0), items.length - 1),
    items
  };
}

/**
 * A list for files and directries.
 *
 * This list only views informations of files and directories in props.
 */
export default class FileList extends React.Component {
  /**
   * Construct FileList component.
   *
   * @param {object} props - properties of component
   */
  constructor(props) {
    super(props);

    this.state = createState(props);
  }

  componentWillReceiveProps(newProps) {
    this.setState(createState(newProps));
  }

  /**
   * @override
   */
  render() {
    return tmpl.apply(this);
  }
}

FileList.defaultProps = {
  current: 0
};
