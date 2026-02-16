import fs from "node:fs";

export function profile(label) {
    console.profile(label);
}

export function profileEnd(label) {
    console.profileEnd(label);
}

// export function appendLog(line) {
//     fs.appendFileSync("debug.log", line + "\n");
// }

export function triggerDebugger() {
    debugger;
}

/**
 * @param {string} path
 * @returns {Promise<string[]>}
 */
export function readdir(path) {
    return new Promise((resolve, reject) =>
        fs.readdir(path, (err, files) => {
            if (err) {
                reject(err);
            } else {
                resolve(files);
            }
        })
    );
}
