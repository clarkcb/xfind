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
    value: any;

    constructor(name: string, type: ArgTokenType, value: any) {
        this.name = name;
        this.type = type;
        this.value = value;
    }
}
