import { Elm } from "./build/main.mjs";
import chokidar from "chokidar";
import fs from "node:fs";
import * as ConcurrentTask from "@andrewmacmurray/elm-concurrent-task";

function setForeground(ansi, text) {
  return "\u001b[" + ansi + "m" + text + "\u001b[39m";
}

var app = Elm.Main.init({ flags: process.argv.slice(2) });

const tasks = {
  log: console.log,
  async die({ message, exitCode }) {
    console.log(setForeground(31 /* red */, "error ") + message);
    process.exit(exitCode);
  },
  getFiles(path) {
    return fs.readdirSync(path, { recursive: true });
  },
  chokidarWatch: (function () {
    /** @type {chokidar.FSWatcher | null} */
    var lastWatch = null;

    return async (targets) => {
      const nextWatch = chokidar
        .watch(targets, { alwaysStat: true })
        .on("all", (eventName, path, stats) => {
          switch (eventName) {
            case "add":
            case "change":
              app.ports.chokidar.send({ eventName, path, stats });
              break;
            default:
              app.ports.chokidar.send({ eventName, path, stats });
          }
        });
      if (lastWatch) await lastWatch.close();
      lastWatch = nextWatch;
    };
  })(),
  getMTime(path) {
    const stat = fs.statSync(path, { throwIfNoEntry: false });
    return stat ? stat.mtimeMs : null;
  },
};

ConcurrentTask.register({
  tasks: tasks,
  ports: {
    send: app.ports.send,
    receive: app.ports.receive,
  },
});
