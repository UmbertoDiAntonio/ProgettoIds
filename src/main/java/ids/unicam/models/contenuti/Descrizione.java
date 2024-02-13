package ids.unicam.models.contenuti;

import ids.unicam.models.attori.TuristaAutenticato;

public class Descrizione  extends MaterialeGenerico {
    public Descrizione(TuristaAutenticato autore, PuntoInteresse puntoInteresse) {
        super(autore,puntoInteresse);
    }
    public Descrizione(TuristaAutenticato autore, Contest contest) {
        super(autore,contest);
    }

    @Override
    public String get() {
        return "Descrizione: "+getId()+", creata da "+ getCreatore().getNome();
    }
}
