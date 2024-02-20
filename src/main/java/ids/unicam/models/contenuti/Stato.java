package ids.unicam.models.contenuti;

public enum Stato {
    APPROVATO,
    NON_APPROVATO;

    public boolean asBoolean() {
        return switch (this) {
            case APPROVATO -> true;
            case NON_APPROVATO -> false;
        };
    }

    public static Stato toStatus(boolean value) {
        return value ? APPROVATO : NON_APPROVATO;
    }
}
