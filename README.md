# SxFiler #
This is a **Simple** and **Extensible** , and having *Two screen* filer created by Electron.

## TODO ##

- [x] implement cancellation for a task in task runner
- [x] Release process for Windows/Linux

## Development ##

### Create new module ###
Run command below on project root.

```bash
$ npx hygen module init <module-name>
```

Result of it create new directory in `src/ts/modules` that is named to input value in command.

### Add new action ###
Run command below on project root.

```bash
$ npx hygen module new-action <module-name>
# then input name of action to want to create for.
```

Result of this command inject action and action creator to module's action module specified.

### Add new command ###
Run command below on project root.

```bash
$ npx hygen command new <command-name>
# then input group of command and command type.
```

Result of this command create module file and test file into under specified command directory.
