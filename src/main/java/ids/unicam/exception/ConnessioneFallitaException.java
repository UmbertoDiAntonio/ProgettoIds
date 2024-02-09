package ids.unicam.exception;

import java.io.IOException;

public class ConnessioneFallitaException extends IOException {
    public ConnessioneFallitaException() {
        super();
    }

    public ConnessioneFallitaException(String message) {
        super(message);
    }

    public ConnessioneFallitaException(String message, Throwable cause) {
        super(message, cause);
    }

    public ConnessioneFallitaException(Throwable cause) {
        super(cause);
    }
}