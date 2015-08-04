/*
 * searchoption.js
 *
 * encapsulates a search option
 */

function SearchOption(shortarg, longarg, desc, func) {
    var self = this;
    self.shortarg = shortarg;
    self.longarg = longarg;
    self.desc = desc;
    self.func = func;

    self.sortarg = (function () {
        if (self.shortarg)
            return self.shortarg.toLowerCase() + 'a' + self.longarg.toLowerCase();
        return self.longarg.toLowerCase();
    })();
}

exports.SearchOption = SearchOption;
