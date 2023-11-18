import { Elm } from "./build/main.mjs";
import chokidar from "chokidar";
import fs from "node:fs";
import * as ConcurrentTask from "@andrewmacmurray/elm-concurrent-task";
import path from "node:path";

var app = Elm.Main.init({ flags: process.argv.slice(2) });

const tasks = {
  log: console.log,
  async exit({ exitCode }) {
    process.exit(exitCode);
  },
  listFiles(dir) {
    return fs
      .readdirSync(dir, { recursive: true })
      .map((name) => path.join(dir, name));
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
  stat(path) {
    return fs.statSync(path, { throwIfNoEntry: false }) || null;
  },
  writeFile(file) {
    const dir = path.dirname(file.path);
    fs.mkdirSync(dir, { recursive: true });
    fs.writeFileSync(file.path, file.content);
  },
};

ConcurrentTask.register({
  tasks: tasks,
  ports: {
    send: app.ports.send,
    receive: app.ports.receive,
  },
});
