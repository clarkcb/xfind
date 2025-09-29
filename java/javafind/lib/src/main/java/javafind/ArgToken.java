/*******************************************************************************
ArgToken

Class to encapsulate a command line argument token

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2025
*******************************************************************************/

package javafind;

public record ArgToken(String name, ArgTokenType type, Object value) {
}
