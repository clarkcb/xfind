package javafind;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class StringUtilTest {
    public StringUtilTest() {}

    @Test
    public final void testTrimNewLineWithNewLine() {
        final var s = "This is a line with a newline\n";
        final var trimmed = StringUtil.trimNewLine(s);
        assertEquals("This is a line with a newline", trimmed);
    }
}
