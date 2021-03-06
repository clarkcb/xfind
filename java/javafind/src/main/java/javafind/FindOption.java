/*******************************************************************************
FindOption

Class to encapsulate a command line find option

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javafind;

public class FindOption {

    private final String shortArg;
    private final String longArg;
    private final String description;

    public FindOption(final String shortArg, final String longArg,
                        final String description) {
        this.shortArg = shortArg;
        this.longArg = longArg;
        this.description = description;
    }

    public final String getShortArg() {
        return this.shortArg;
    }

    public final String getLongArg() {
        return this.longArg;
    }

    public final String getDescription() {
        return this.description;
    }

    public final String getSortArg() {
        if (null != this.shortArg && !this.shortArg.isEmpty()) {
            return this.shortArg.toLowerCase() + "@" + this.longArg;
        }
        return this.longArg;
    }
}
