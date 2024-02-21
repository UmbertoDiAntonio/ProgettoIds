package ids.unicam.models.contenuti.notifiche;

import ids.unicam.models.attori.TuristaAutenticato;

public class NotificaBuilder {
    private String titolo;
    private String descrizione;
    private String usernameDestinatario;

    public NotificaBuilder() {}

    public NotificaBuilder withTitolo(String titolo) {
        this.titolo = titolo;
        return this;
    }

    public NotificaBuilder withDescrizione(String descrizione) {
        this.descrizione = descrizione;
        return this;
    }

    public NotificaBuilder withDestinatario(TuristaAutenticato destinatario) {
        this.usernameDestinatario = destinatario.getUsername();
        return this;
    }

    /**
     * Valida i dati inseriti e restituisce un istanza della classe generata
     * @return la notifica generata
     */
    public Notifica build() {
        if (titolo == null || titolo.isBlank() || descrizione == null || descrizione.isBlank()) {
            throw new IllegalArgumentException("Titolo o descrizione sono vuoti");
        }
        if(usernameDestinatario == null || usernameDestinatario.isBlank()){
            throw new IllegalArgumentException("E' necessario impostare un destinatario valido");
        }
        return new Notifica(titolo, descrizione, usernameDestinatario);
    }
}
