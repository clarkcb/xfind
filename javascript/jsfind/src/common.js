/*
 * common.js
 *
 * Some common functions, etc.
 */

const {COLORS} = require('./color');

// add a startsWith method to String type
if (typeof String.prototype.startsWith !== 'function') {
    String.prototype.startsWith = (str) => {
        return this.slice(0, str.length) === str;
    };
}

const log = (message) => console.log(message);

const logError = (message, colorize = true) => {
    if (colorize) {
        process.stderr.write(`${COLORS.BOLD_RED}${message}${COLORS.RESET}\n`);
    } else {
        process.stderr.write(message + "\n");
    }
}

const boolHashFromArray = (arr) => {
    const hash = {};
    arr.forEach(a => hash[a] = true);
    return hash;
};

const setFromArray = (arr) => {
    let hash = boolHashFromArray(arr);
    let set = [];
    for (let k in hash) {
        if (Object.prototype.hasOwnProperty.call(hash, k)) {
            set.push(k);
        }
    }
    return set;
};

module.exports = {log, logError, boolHashFromArray, setFromArray};
