/*
 * findoption.js
 *
 * encapsulates a find option
 */

class FindOption {
    'use strict'

    constructor(shortArg, longArg, desc, func) {
        this.shortArg = shortArg;
        this.longArg = longArg;
        this.desc = desc;
        this.func = func;

        this.sortArg = (() => {
            if (this.shortArg)
                return this.shortArg.toLowerCase() + 'a' + this.longArg.toLowerCase();
            return this.longArg.toLowerCase();
        })();
    }
}

exports.FindOption = FindOption;
