package ids.unicam.exception;

public class ContestException extends Exception {
    public ContestException() {
        super();
    }

    public ContestException(String message) {
        super(message);
    }

    public ContestException(String message, Throwable cause) {
        super(message, cause);
    }

    public ContestException(Throwable cause) {
        super(cause);
    }
}
