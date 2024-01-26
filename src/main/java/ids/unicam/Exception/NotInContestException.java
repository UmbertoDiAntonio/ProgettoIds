package ids.unicam.Exception;

public class NotInContestException extends RuntimeException {
    public NotInContestException() {
        super();
    }

    public NotInContestException(String message) {
        super(message);
    }

    public NotInContestException(String message, Throwable cause) {
        super(message, cause);
    }

    public NotInContestException(Throwable cause) {
        super(cause);
    }
}
