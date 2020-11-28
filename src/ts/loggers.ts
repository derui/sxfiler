/**
 * This module provides function to initialize logger in this application
 */
import * as winston from "winston";
import { TransformableInfo } from "logform";
import TransportStream from "winston-transport";

export const Loggers = {
  RPC: "RPC",
  COMMAND: "Command",
  KEY_EVENT: "KeyEvent",
} as const;

// borrows implementation from https://github.com/winstonjs/winston/issues/287
// enumeration to assign color values to
enum LevelColors {
  INFO = "darkturquoise",
  WARN = "khaki",
  ERROR = "tomato",
}

// type levels used for setting color and shutting typescript up
type Levels = "INFO" | "WARN" | "ERROR";

const defaultColor = "color: inherit";

//! Overriding winston console transporter
class Console extends TransportStream {
  constructor(options = {}) {
    super(options);

    this.setMaxListeners(30);
  }

  log(info: TransformableInfo, next: () => void) {
    // styles a console log statement accordingly to the log level
    // log level colors are taken from levelcolors enum
    console.log(
      `%c[%c${info.level.toUpperCase()}%c]:`,
      defaultColor,
      `color: ${LevelColors[info.level.toUpperCase() as Levels]};`,
      defaultColor,
      // message will be included after stylings
      // through this objects and arrays will be expandable
      info.message
    );

    // must call the next function here
    // or otherwise you'll only be able to send one message
    next();
  }
}

export const initializeLoggers = function initializeLoggers(debug = false) {
  const format = winston.format;
  // logger for RPC
  winston.loggers.add(Loggers.RPC, {
    level: debug ? "debug" : "info",
    format: format.combine(format.label({ label: "RPC" }), format.timestamp(), format.json()),
    transports: [new Console()],
  });

  // logger for command
  winston.loggers.add(Loggers.COMMAND, {
    level: debug ? "debug" : "info",
    format: format.combine(format.label({ label: "Command" }), format.timestamp(), format.json()),
    transports: [new Console()],
  });

  // logger for key event
  winston.loggers.add(Loggers.KEY_EVENT, {
    level: debug ? "debug" : "info",
    format: format.combine(format.label({ label: "Key Event" }), format.timestamp(), format.json()),
    transports: [new Console()],
  });
};
