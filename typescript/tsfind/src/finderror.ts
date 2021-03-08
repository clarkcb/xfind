/*
 * finderror.ts
 *
 * custom error class for tsfind
 */

export class FindError extends Error {
    constructor(message: string) {
        super(message);
        this.name = 'FindError';
    }
}
