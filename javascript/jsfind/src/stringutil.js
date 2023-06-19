/*
 * stringutil.js
 *
 * string-related utility functions
 */

// 'use strict'

const dateToString = (name, dt) => {
    let s = `${name}=`;
    if (dt === null)
        s += '0';
    else
        s += `"${dt.toISOString()}"`;
    return s;
};

const getDateForString = (s) => {
    const d = new Date();
    d.setTime(Date.parse(s));
    return d;
};

const listToString = (name, lst) => {
    if (lst.length) return `${name}=["${lst.join('","')}"]`;
    return `${name}=[]`;
};

module.exports = {dateToString, getDateForString, listToString};
