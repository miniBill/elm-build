import { Elm } from "./build/main.mjs";
import chokidar from "chokidar";

function setForeground(ansi, text) {
  return "\u001b[" + ansi + "m" + text + "\u001b[39m";
}

let color = {
  black: (text) => setForeground(30, text),
  red: (text) => setForeground(31, text),
  green: (text) => setForeground(32, text),
  yellow: (text) => setForeground(33, text),
  blue: (text) => setForeground(34, text),
  magenta: (text) => setForeground(35, text),
  cyan: (text) => setForeground(36, text),
  white: (text) => setForeground(37, text),
  brightBlack: (text) => setForeground(90, text),
  brightRed: (text) => setForeground(91, text),
  brightGreen: (text) => setForeground(92, text),
  brightYellow: (text) => setForeground(93, text),
  brightBlue: (text) => setForeground(94, text),
  brightMagenta: (text) => setForeground(95, text),
  brightCyan: (text) => setForeground(96, text),
  brightWhite: (text) => setForeground(97, text),
};

console.black = (text) => console.log(color.black(text));
console.red = (text) => console.log(color.red(text));
console.green = (text) => console.log(color.green(text));
console.yellow = (text) => console.log(color.yellow(text));
console.blue = (text) => console.log(color.blue(text));
console.magenta = (text) => console.log(color.magenta(text));
console.cyan = (text) => console.log(color.cyan(text));
console.white = (text) => console.log(color.white(text));
console.brightBlack = (text) => console.log(color.brightBlack(text));
console.brightRed = (text) => console.log(color.brightRed(text));
console.brightGreen = (text) => console.log(color.brightGreen(text));
console.brightYellow = (text) => console.log(color.brightYellow(text));
console.brightBlue = (text) => console.log(color.brightBlue(text));
console.brightMagenta = (text) => console.log(color.brightMagenta(text));
console.brightCyan = (text) => console.log(color.brightCyan(text));
console.brightWhite = (text) => console.log(color.brightWhite(text));

console.clear();

console.cyan("Starting elm-build");

console.log("  Setting up chokidar");

const watchTarget = process.argv.slice(2) + "/src";
console.brightBlack("    Watching " + watchTarget);
const watch = chokidar.watch(watchTarget).on("all", (event, path) => {
  app.ports.chokidar.send({ event, path });
});

console.log("  Starting the Elm app");
var app = Elm.Main.init({ flags: process.argv.slice(2) });

function subscribe(portName, callback) {
  if (portName in app.ports) app.ports[portName].subscribe(callback);
}

subscribe("log", function (line) {
  console.log("  " + line);
});
subscribe("die", async function (code) {
  console.cyan("\nStopping elm-build");
  console.log("  Stopping chokidar");
  await watch.close();
  console.log("  Exiting");
  process.exit(code);
});

console.cyan("\nRunning");
