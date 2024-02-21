package ids.unicam.models.contenuti;

import ids.unicam.models.attori.TuristaAutenticato;

public class NotificaBuilder {
    private String titolo;
    private String descrizione;
    private TuristaAutenticato ricevente;

    public NotificaBuilder() {}

    public NotificaBuilder withTitolo(String titolo) {
        this.titolo = titolo;
        return this;
    }

    public NotificaBuilder withDescrizione(String descrizione) {
        this.descrizione = descrizione;
        return this;
    }

    public NotificaBuilder withRicevente(TuristaAutenticato ricevente) {
        this.ricevente = ricevente;
        return this;
    }

    public Notifica build() {
        if (titolo == null || titolo.isBlank() || descrizione == null || descrizione.isBlank()) {
            throw new IllegalArgumentException("Titolo o descrizione sono vuoti");
        }
        //TODO validare ricevente
        return new Notifica(titolo, descrizione, ricevente);
    }
}
