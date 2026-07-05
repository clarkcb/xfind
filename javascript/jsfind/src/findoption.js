/*
 * findoption.js
 *
 * encapsulates a find option
 */

class FindOption {
    constructor(shortArg, longArg, desc, argType) {
        this.shortArg = shortArg;
        this.longArg = longArg;
        this.desc = desc;
        this.argType = argType;

        this.sortArg = (() => {
            if (this.shortArg)
                return this.shortArg.toLowerCase() + 'a' + this.longArg.toLowerCase();
            return this.longArg.toLowerCase();
        })();
    }
}

exports.FindOption = FindOption;
