package ids.unicam.models.contenuti.notifiche;

import ids.unicam.models.attori.TuristaAutenticato;

/**
 * Builder per la creazione di oggetti Notifica.
 */
public class NotificaBuilder {
    private String titolo;
    private String descrizione;
    private String usernameDestinatario;


    public NotificaBuilder() {
    }

    /**
     * Imposta il titolo della notifica.
     *
     * @param titolo Il titolo della notifica.
     * @return Restituisce l'istanza corrente del NotificaBuilder.
     */
    public NotificaBuilder withTitolo(String titolo) {
        this.titolo = titolo;
        return this;
    }

    /**
     * Imposta la descrizione della notifica.
     *
     * @param descrizione La descrizione della notifica.
     * @return Restituisce l'istanza corrente del NotificaBuilder.
     */
    public NotificaBuilder withDescrizione(String descrizione) {
        this.descrizione = descrizione;
        return this;
    }

    /**
     * Imposta il destinatario della notifica.
     *
     * @param destinatario Il destinatario della notifica.
     * @return Restituisce l'istanza corrente del NotificaBuilder.
     */
    public NotificaBuilder withDestinatario(TuristaAutenticato destinatario) {
        this.usernameDestinatario = destinatario.getUsername();
        return this;
    }

    /**
     * Valida i dati inseriti e restituisce un'istanza della classe Notifica.
     *
     * @return La notifica generata.
     * @throws IllegalArgumentException se titolo o descrizione sono vuoti, o se il destinatario non è valido.
     */
    public Notifica build() {
        if (titolo == null || titolo.isBlank() || descrizione == null || descrizione.isBlank()) {
            throw new IllegalArgumentException("Titolo o descrizione sono vuoti");
        }
        if (usernameDestinatario == null || usernameDestinatario.isBlank()) {
            throw new IllegalArgumentException("E' necessario impostare un destinatario valido");
        }
        return new Notifica(titolo, descrizione, usernameDestinatario);
    }
}
