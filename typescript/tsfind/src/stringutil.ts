import {FileType} from "./filetype";
import {FileTypes} from "./filetypes";

export class StringUtil {
    public static dateToString(name: string, dt: Date | null): string {
        let s = `${name}=`;
        if (dt === null)
            s += '0';
        else
            s += `"${dt.toISOString()}"`;
        return s;
    }

    public static getDateForString(s: string): Date {
        const d = new Date();
        d.setTime(Date.parse(s));
        return d;
    }

    public static stringListToString(name: string, lst: string[]): string {
        let s = `${name}=[`;
        if (lst.length) {
            s += `"${lst.join('", "')}"`;
        }
        s += ']';
        return s;
    }

    public static patternListToString(name: string, lst: RegExp[]): string {
        let s = `${name}=[`;
        if (lst.length) {
            const ps = lst.map(p => p.source);
            s += `"${ps.join('", "')}"`;
        }
        s += ']';
        return s;
    }

    public static fileTypesToString(name: string, fileTypes: FileType[]): string {
        let s = `${name}=[`;
        for (let i=0; i < fileTypes.length; i++) {
            if (i > 0) s += ', ';
            s += FileTypes.toName(fileTypes[i]);
        }
        s += ']';
        return s;
    }


}