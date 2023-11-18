import { Elm } from "./build/main.mjs";
import chokidar from "chokidar";
import fs from "node:fs";
import * as ConcurrentTask from "@andrewmacmurray/elm-concurrent-task";
import path from "node:path";

const __filename = new URL(import.meta.url).pathname;
var filenameIndex = process.argv.indexOf(__filename);
const flags =
  filenameIndex > 0 ? process.argv.slice(filenameIndex + 1) : process.argv;

var app = Elm.Main.init({ flags });

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
