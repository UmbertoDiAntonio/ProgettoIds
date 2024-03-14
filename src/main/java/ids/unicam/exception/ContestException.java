package ids.unicam.exception;

import static ids.unicam.Main.logger;

public class ContestException extends Exception {
    public ContestException() {
        super();
    }

    public ContestException(String message) {
        super(message);
        logger.warn(message);
    }

    public ContestException(String message, Throwable cause) {
        super(message, cause);
    }

    public ContestException(Throwable cause) {
        super(cause);
    }
}
