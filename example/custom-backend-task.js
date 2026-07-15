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

export async function writeBinaryFile({ path, body }, { cwd }) {
    const resolved = nodePath.resolve(cwd, path);
    const data = Buffer.from(body, "hex");
    await fs.writeFile(resolved, data);
}

/**
 * @param {string} path
 * @returns {Promise<{ files : string[]; directories: string[]; }>}
 */
export async function readdir(path) {
    debugger;
    let list = await fs.readdir(path, { withFileTypes: true });
    let files = [];
    let directories = [];
    for (let dirent of list) {
        if (dirent.isDirectory()) {
            directories.push(dirent.name);
        } else {
            files.push(dirent.name);
        }
    }
    return { files, directories };
}

/**
 * @param {string} path
 * @returns {string}
 */
export function resolve(path) {
    return nodePath.resolve(path);
}
