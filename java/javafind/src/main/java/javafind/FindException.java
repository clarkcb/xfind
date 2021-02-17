package javafind;

public class FindException extends Exception {

    public FindException() {
        super();
    }

    public FindException(final String msg) {
        super(msg);
    }

    public FindException(final String msg, final Exception cause) {
        super(msg, cause);
    }
}
