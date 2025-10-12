/*******************************************************************************
FindOption

Class to encapsulate a command line find option

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javafind;

public interface Option {
    String shortArg();
    String longArg();
    String description();
    ArgTokenType argType();
}
