package ids.unicam.models.contenuti.notifiche;

import ids.unicam.models.attori.TuristaAutenticato;
import org.jetbrains.annotations.NotNull;

public class NotificaBuilder {
    private String titolo;
    private String descrizione;
    private String usernameDestinatario;

    public NotificaBuilder() {
    }

    public  @NotNull NotificaBuilder withTitolo( @NotNull String titolo) {
        this.titolo = titolo;
        return this;
    }

    public  @NotNull NotificaBuilder withDescrizione( @NotNull String descrizione) {
        this.descrizione = descrizione;
        return this;
    }

    public @NotNull NotificaBuilder withDestinatario( @NotNull TuristaAutenticato destinatario) {
        this.usernameDestinatario = destinatario.getUsername();
        return this;
    }

    /**
     * Valida i dati inseriti e restituisce un istanza della classe generata
     *
     * @return la notifica generata
     */
    public @NotNull Notifica build() {
        if (titolo == null || titolo.isBlank() || descrizione == null || descrizione.isBlank()) {
            throw new IllegalArgumentException("Titolo o descrizione sono vuoti");
        }
        if (usernameDestinatario == null || usernameDestinatario.isBlank()) {
            throw new IllegalArgumentException("E' necessario impostare un destinatario valido");
        }
        return new Notifica(titolo, descrizione, usernameDestinatario);
    }
}
