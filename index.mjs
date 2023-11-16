import { Elm } from "./build/main.mjs";
import chokidar from "chokidar";
import crypto from "node:crypto";
import fs from "node:fs";

function setForeground(ansi, text) {
  return "\u001b[" + ansi + "m" + text + "\u001b[39m";
}

let color = {
  red: 31,
  cyan: 36,
};

console.clear();

console.log(setForeground(color.cyan, "info  ") + "Starting elm-build");
var app = Elm.Main.init({ flags: process.argv.slice(2) });

function subscribe(portName, callback) {
  if (portName in app.ports) app.ports[portName].subscribe(callback);
}

subscribe("log", console.log);
subscribe("die", async ({ message, exitCode }) => {
  console.log(setForeground(color.red, "error ") + message);
  process.exit(exitCode);
});
subscribe("chokidarWatch", (target) => {
  chokidar.watch(target).on("all", (eventName, path) => {
    switch (eventName) {
      case "add":
      case "change":
        const data = fs.readFileSync(path);
        const hash = crypto.createHash("sha1").update(data).digest("base64");
        app.ports.chokidar.send({ eventName, path, hash });
        break;
      default:
        app.ports.chokidar.send({ eventName, path });
    }
  });
});
