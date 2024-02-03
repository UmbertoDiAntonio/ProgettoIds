package ids.unicam.utilites;

public enum Stato {
    APPROVED,
    NOT_APPROVED;

    public boolean asBoolean() {
        return switch (this) {
            case APPROVED -> true;
            case NOT_APPROVED -> false;
        };
    }

    public static Stato toStatus(boolean value) {
        return value ? APPROVED : NOT_APPROVED;
    }
}
