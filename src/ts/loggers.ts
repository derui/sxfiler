/**
 * This module provides function to initialize logger in this application
 */
import * as winston from "winston";

export const Loggers = {
  RPC: "RPC",
  COMMAND: "Command",
  KEY_EVENT: "KeyEvent",
} as const;

export const initializeLoggers = function initializeLoggers(debug = false) {
  const format = winston.format;
  // logger for RPC
  winston.loggers.add(Loggers.RPC, {
    level: debug ? "debug" : "info",
    format: format.combine(format.label({ label: "RPC" }), format.timestamp(), format.json()),
    transports: [new winston.transports.Console()],
  });

  // logger for command
  winston.loggers.add(Loggers.COMMAND, {
    level: debug ? "debug" : "info",
    format: format.combine(format.label({ label: "Command" }), format.timestamp(), format.json()),
    transports: [new winston.transports.Console()],
  });

  // logger for key event
  winston.loggers.add(Loggers.KEY_EVENT, {
    level: debug ? "debug" : "info",
    format: format.combine(format.label({ label: "Key Event" }), format.timestamp(), format.json()),
    transports: [new winston.transports.Console()],
  });
};
