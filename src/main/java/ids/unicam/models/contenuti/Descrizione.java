package ids.unicam.models.contenuti;

import ids.unicam.models.attori.TuristaAutenticato;

public class Descrizione  extends MaterialeGenerico {
    public Descrizione(TuristaAutenticato autore) {
        super(autore);
    }

    @Override
    public String get() {
        return "Descrizione: "+getId()+", creata da "+ getCreatore().getNome();
    }
}
