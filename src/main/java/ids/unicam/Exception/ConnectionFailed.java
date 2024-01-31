package ids.unicam.Exception;

import java.io.IOException;

public class ConnectionFailed extends IOException {
    public ConnectionFailed() {
        super();
    }

    public ConnectionFailed(String message) {
        super(message);
    }

    public ConnectionFailed(String message, Throwable cause) {
        super(message, cause);
    }

    public ConnectionFailed(Throwable cause) {
        super(cause);
    }
}