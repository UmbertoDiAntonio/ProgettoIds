package ids.unicam.models.contenuti;

import org.jetbrains.annotations.Nullable;

public enum Stato {
    APPROVATO,
    NON_APPROVATO,
    IN_ATTESA;

    /**
     * Effettua il casting da Boolean a uno Stato
     *
     * @param value il valore Boolean da convertire
     * @return lo stato corrispondente
     * @see Boolean
     */
    public static Stato toStatus(Boolean value) {
        return value == null ? IN_ATTESA : value ? APPROVATO : NON_APPROVATO;
    }

    /**
     * Effettua il casting a Boolean di uno Stato
     *
     * @return il valore Boolean corrispondente
     */
    public @Nullable Boolean asBoolean() {
        return switch (this) {
            case APPROVATO -> true;
            case NON_APPROVATO -> false;
            case IN_ATTESA -> null;
        };
    }


}
