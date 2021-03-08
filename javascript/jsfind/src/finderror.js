/*
 * finderror.js
 *
 * custom error class for jsfind
 */

class FindError extends Error {
    constructor(message) {
        super(message);
        this.name = 'FindError';
    }
}

exports.FindError = FindError;
