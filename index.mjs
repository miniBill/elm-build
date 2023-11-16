import { Elm } from "./build/main.mjs";
import chokidar from "chokidar";
import crypto from "node:crypto";
import fs from "node:fs";
import * as ConcurrentTask from "@andrewmacmurray/elm-concurrent-task";

function setForeground(ansi, text) {
  return "\u001b[" + ansi + "m" + text + "\u001b[39m";
}

if ("--digest" in process.argv) {
  console.log("YOLO");
  process.exit(0);
}

var app = Elm.Main.init({ flags: process.argv.slice(2) });

const tasks = {
  log: console.log,
  die: async ({ message, exitCode }) => {
    console.log(setForeground(31 /* red */, "error ") + message);
    process.exit(exitCode);
  },
  chokidarWatch: (function () {
    /** @type {chokidar.FSWatcher | null} */
    var lastWatch = null;

    return async ({ targets, digest }) => {
      const nextWatch = chokidar
        .watch(targets, { alwaysStat: true })
        .on("all", (eventName, path, stats) => {
          switch (eventName) {
            case "add":
            case "change":
              let hash = null;
              if (digest) {
                const data = fs.readFileSync(path);
                hash = crypto.createHash("sha1").update(data).digest("base64");
              }
              app.ports.chokidar.send({ eventName, path, stats, hash });
              break;
            default:
              app.ports.chokidar.send({ eventName, path, stats });
          }
        });
      if (lastWatch) await lastWatch.close();
      lastWatch = nextWatch;
    };
  })(),
};

ConcurrentTask.register({
  tasks: tasks,
  ports: {
    send: app.ports.send,
    receive: app.ports.receive,
  },
});
