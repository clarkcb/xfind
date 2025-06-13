/*******************************************************************************
FindOption

Class to encapsulate a command line find option

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javafind;

public record FindOption(String shortArg, String longArg, String description) {

    public String getSortArg() {
        if (null != this.shortArg && !this.shortArg.isEmpty()) {
            return this.shortArg.toLowerCase() + "@" + this.longArg;
        }
        return this.longArg;
    }
}
