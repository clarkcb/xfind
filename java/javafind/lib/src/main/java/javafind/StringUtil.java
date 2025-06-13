/* *****************************************************************************
FileUtil

Utility class to determine file types, etc.

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
***************************************************************************** */

package javafind;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public final class StringUtil {
    private StringUtil() {
        // inaccessible constructor for utility class
    }

    public static final Set<Character> NEWLINECHARS = new HashSet<>(Arrays.asList('\n', '\r'));

    public static String trimNewLine(final String s) {
        var trimmed = s;
        while (!trimmed.isEmpty() && NEWLINECHARS.contains(trimmed.charAt(trimmed.length() - 1))) {
            trimmed = trimmed.substring(0, trimmed.length() - 1);
        }
        return trimmed;
    }
}
