import fs from "node:fs/promises";

export function profile(label) {
    console.profile(label);
}

export function profileEnd(label) {
    console.profileEnd(label);
}

export function triggerDebugger() {
    debugger;
}

/**
 * @param {string} path
 * @returns {Promise<string[]>}
 */
export function readdir(path) {
    return fs.readdir(path);
}

export async function fileExists(path) {
    try {
        await fs.stat(path, {});
        return true;
    } catch {
        return false;
    }
}
