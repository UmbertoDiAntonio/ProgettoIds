package ids.unicam.utilites;

public enum Stato {
    APPROVED(true),
    NOT_APPROVED(false);
    private final boolean approvato;


    Stato(boolean approvato) {
        this.approvato = approvato;
    }

    public boolean getApprovato() {
        return approvato;
    }
}
