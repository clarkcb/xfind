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

module.exports = {dateToString, getDateForString, timestampToString, getTimestampForString, patternListToString, stringListToString};
