import fs from "node:fs/promises";
import * as nodePath from "path";

export function profile(label) {
    console.profile(label);
}

export function profileEnd(label) {
    console.profileEnd(label);
}

export function triggerDebugger() {
    debugger;
}

export async function writeBinaryFile({ path, body, cwd }) {
    const resolved = nodePath.resolve(cwd, path);
    const data = Buffer.from(body, "hex");
    await fs.writeFile(resolved, data);
}

/**
 * @param {string} path
 * @returns {Promise<string[]>}
 */
export function readdir(path) {
    return fs.readdir(path);
}
