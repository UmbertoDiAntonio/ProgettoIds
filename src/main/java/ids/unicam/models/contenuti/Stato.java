package ids.unicam.models.contenuti;

import org.jetbrains.annotations.Nullable;

public enum Stato {
    APPROVATO,
    NON_APPROVATO,
    IN_ATTESA;

    public static Stato toStatus(Boolean value) {
        return value == null ? IN_ATTESA : value ? APPROVATO : NON_APPROVATO;
    }

    public @Nullable Boolean asBoolean() {
        return switch (this) {
            case APPROVATO -> true;
            case NON_APPROVATO -> false;
            case IN_ATTESA -> null;
        };
    }


}
