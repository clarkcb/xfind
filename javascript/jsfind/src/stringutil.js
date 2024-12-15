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

const timestampToString = (name, ts) => {
    let s = `${name}=`;
    if (!ts)
        s += '0';
    else {
        const dt = new Date();
        dt.setTime(ts);
        s += `"${dt.toISOString()}"`;
    }
    return s;
};

const getDateForString = (s) => {
    const d = new Date();
    d.setTime(Date.parse(s));
    return d;
};

const getTimestampForString = (s) => {
    return Date.parse(s);
};

const stringListToString = (name, lst) => {
    let s = `${name}=[`;
    if (lst.length) s += `"${lst.join('", "')}"`;
    s += ']';
    return s;
};

const patternListToString = (name, lst) => {
    let s = `${name}=[`;
    if (lst.length) {
        const ps = lst.map(p => p.source);
        s += `"${ps.join('", "')}"`;
    }
    s += ']';
    return s;
};

const trimFromEnd = (s, chars) => {
    let i = s.length - 1;
    while (i >= 0 && chars.indexOf(s[i]) >= 0) {
        i--;
    }
    return s.substring(0, i+1);
};

module.exports = {dateToString, getDateForString, timestampToString, getTimestampForString, patternListToString,
    stringListToString, trimFromEnd};
