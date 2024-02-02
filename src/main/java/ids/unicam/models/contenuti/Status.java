package ids.unicam.models.contenuti;

public enum Status {
    APPROVED(true),
    NOT_APPROVED(false);
    private final boolean approvato;


    Status(boolean approvato) {
        this.approvato = approvato;
    }

    public boolean getApprovato() {
        return approvato;
    }
}
