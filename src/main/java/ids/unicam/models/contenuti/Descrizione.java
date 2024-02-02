package ids.unicam.models.contenuti;

import ids.unicam.models.attori.TuristaAutenticato;

public class Descrizione  extends Materiale{
    public Descrizione(TuristaAutenticato autore) {
        super(autore);
    }

    @Override
    public String get() {
        return "Questa è una descrizione, creata da "+super.getCreatore().getNome()+", con id: "+super.getId();
    }
}
