/*
 * common.js
 *
 * Some common functions, etc.
 */

// add a startsWith method to String type
if (typeof String.prototype.startsWith !== 'function') {
    String.prototype.startsWith = (str) => {
        return this.slice(0, str.length) === str;
    };
}

const log = (message) => console.log(message);

const boolHashFromArray = (arr) => {
    const hash = {};
    arr.forEach(a => hash[a] = true);
    return hash;
};

const setFromArray = (arr) => {
    let hash = boolHashFromArray(arr);
    let set = [];
    for (let k in hash) {
        if (hash.hasOwnProperty(k)) {
            set.push(k);
        }
    }
    return set;
};

module.exports = {log, boolHashFromArray, setFromArray};
