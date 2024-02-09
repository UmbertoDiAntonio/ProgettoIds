package ids.unicam.exception;

public class RegistrazioneException extends RuntimeException{

    public RegistrazioneException() {
        super();
    }

    public RegistrazioneException(String message) {
        super(message);
    }

    public RegistrazioneException(String message, Throwable cause) {
        super(message, cause);
    }

    public RegistrazioneException(Throwable cause) {
        super(cause);
    }
}
