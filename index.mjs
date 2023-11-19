import { Elm } from "./build/main.mjs";
import chokidar from "chokidar";
import fs from "node:fs";
import * as ConcurrentTask from "@andrewmacmurray/elm-concurrent-task";
import path from "node:path";
import child_process from "node:child_process";

const __filename = new URL(import.meta.url).pathname;
var filenameIndex = process.argv.indexOf(__filename);
const flags =
  filenameIndex > 0 ? process.argv.slice(filenameIndex + 1) : process.argv;

var app = Elm.Main.init({
  flags: { argv: process.argv, versionMessage: "0.1" },
});

app.ports.printAndExitFailure.subscribe((message) => {
  console.log(message);
  process.exit(1);
});
app.ports.printAndExitSuccess.subscribe((message) => {
  console.log(message);
  process.exit(0);
});

function promisify(withCb) {
  return new Promise((resolve, reject) =>
    withCb((err, files) => (err ? reject(err) : resolve(files)))
  );
}

const tasks = {
  log: console.log,
  exit({ exitCode }) {
    process.exit(exitCode);
  },
  async command(args) {
    let result = await new Promise((resolve, reject) => {
      var stdout = [];
      var stderr = [];
      const process = child_process.spawn(args[0], args.slice(1));
      process.stdout.on("data", stdout.push);
      process.stderr.on("data", stderr.push);
      process.on("error", reject);
      process.on("close", (exitCode) => resolve({ exitCode, stdout, stderr }));
    });
    debugger;
    return result;
  },
  async listFiles(dir) {
    let names = await promisify((cb) =>
      fs.readdir(dir, { recursive: true }, cb)
    );
    return names.map((name) => path.join(dir, name));
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
  async stat(path) {
    try {
      const result = await promisify((cb) =>
        fs.stat(path, { throwIfNoEntry: false }, cb)
      );
      return result || null;
    } catch {
      return null;
    }
  },
  async writeFile(file) {
    const dir = path.dirname(file.path);
    await promisify((cb) => fs.mkdir(dir, { recursive: true }, cb));
    await promisify((cb) => fs.writeFile(file.path, file.content, cb));
  },
  async readBinary(file) {
    const binary = await promisify((cb) => fs.readFile(file, cb));
    return binary.toString("base64");
  },
};

ConcurrentTask.register({
  tasks: tasks,
  ports: {
    send: app.ports.send,
    receive: app.ports.receive,
  },
});
