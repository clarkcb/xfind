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

    public static timestampToString(name: string, ts: number): string {
        let s = `${name}=`;
        if (!ts)
            s += '0';
        else {
            const dt = new Date();
            dt.setTime(ts);
            s += `"${dt.toISOString()}"`;
        }
        return s;
    }

    public static getTimestampForString(s: string): number {
        return Date.parse(s);
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

    public static trimFromEnd(s: string, chars: string): string {
        let i = s.length - 1;
        while (i >= 0 && chars.indexOf(s[i]) >= 0) {
            i--;
        }
        return s.substring(0, i + 1);
    }
}