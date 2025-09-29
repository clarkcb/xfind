/*
 * argtoken.js
 *
 * encapsulates an argument token
 */

const {ArgTokenType} = require("./argtokentype");

class ArgToken {
    constructor(name, type, value) {
        this.name = name;
        this.type = type;
        this.value = value;
    }
}

exports.ArgToken = ArgToken;
