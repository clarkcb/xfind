/*
 * argtoken.js
 *
 * encapsulates an argument token
 */

'use strict';

import {ArgTokenType} from "./argtokentype";

export class ArgToken {
    name: string;
    type: ArgTokenType;
    value: boolean | number | string | string[];

    constructor(name: string, type: ArgTokenType, value: boolean | number | string | string[]) {
        this.name = name;
        this.type = type;
        this.value = value;
    }
}
