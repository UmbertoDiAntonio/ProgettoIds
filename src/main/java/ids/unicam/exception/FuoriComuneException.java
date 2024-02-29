package ids.unicam.exception;

import static ids.unicam.Main.logger;

public class FuoriComuneException extends Exception {
    public FuoriComuneException(String message) {
        super(message);
        logger.error(message);
    }
}
