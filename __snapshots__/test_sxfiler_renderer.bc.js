exports['File size component should show size of the file 1'] = {
  "type": "span",
  "key": null,
  "ref": null,
  "props": {
    "className": "fp-FileItem_FileSize",
    "children": "   0.0B"
  },
  "_owner": null,
  "_store": {}
}

exports['File size component should show kilobyte if the size more than 1024 byte 1'] = {
  "type": "span",
  "key": null,
  "ref": null,
  "props": {
    "className": "fp-FileItem_FileSize",
    "children": "   1.0K"
  },
  "_owner": null,
  "_store": {}
}

exports['File size component should show megabyte if the size more than 1024 KBytes 1'] = {
  "type": "span",
  "key": null,
  "ref": null,
  "props": {
    "className": "fp-FileItem_FileSize",
    "children": "   1.0M"
  },
  "_owner": null,
  "_store": {}
}

exports['File size component should show Gigabytes if the size more than 1024 MBytes 1'] = {
  "type": "span",
  "key": null,
  "ref": null,
  "props": {
    "className": "fp-FileItem_FileSize",
    "children": "   1.0G"
  },
  "_owner": null,
  "_store": {}
}

exports['File mode component should show current permission of file 1'] = {
  "type": "span",
  "key": null,
  "ref": null,
  "props": {
    "className": "fp-FileItem_FileMode",
    "children": "-rw-r--r--"
  },
  "_owner": null,
  "_store": {}
}

exports['File mode component should be hyphen if no any permission 1'] = {
  "type": "span",
  "key": null,
  "ref": null,
  "props": {
    "className": "fp-FileItem_FileMode",
    "children": "----------"
  },
  "_owner": null,
  "_store": {}
}

exports['File mode component should be able to show symlink bit if mode contains symlink bit 1'] = {
  "type": "span",
  "key": null,
  "ref": null,
  "props": {
    "className": "fp-FileItem_FileMode",
    "children": "lrwxrwxrwx"
  },
  "_owner": null,
  "_store": {}
}

exports['File mode component should be able to show directory bit if mode contains directory bit 1'] = {
  "type": "span",
  "key": null,
  "ref": null,
  "props": {
    "className": "fp-FileItem_FileMode",
    "children": "drwxr-xr-x"
  },
  "_owner": null,
  "_store": {}
}

exports['File name component should who filename normally 1'] = {
  "type": "span",
  "key": null,
  "ref": null,
  "props": {
    "className": "fp-FileItem_FileName",
    "children": "sample.txt"
  },
  "_owner": null,
  "_store": {}
}

exports['File name component should make another color if item is directory 1'] = {
  "type": "span",
  "key": null,
  "ref": null,
  "props": {
    "className": "fp-FileItem_FileName fp-FileItem_FileName-directory",
    "children": "bar/"
  },
  "_owner": null,
  "_store": {}
}

exports['File name component should make another color if item is symbolic link 1'] = {
  "type": "span",
  "key": null,
  "ref": null,
  "props": {
    "className": "fp-FileItem_FileName fp-FileItem_FileName-symlink",
    "children": "sample.txt"
  },
  "_owner": null,
  "_store": {}
}
