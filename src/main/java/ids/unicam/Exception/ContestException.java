package ids.unicam.Exception;

public class ContestException extends RuntimeException {
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
