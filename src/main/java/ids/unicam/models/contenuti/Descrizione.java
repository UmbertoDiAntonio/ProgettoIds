package ids.unicam.models.contenuti;

import ids.unicam.models.attori.TuristaLoggato;

public class Descrizione  extends Materiale{
    public Descrizione(boolean approved, TuristaLoggato autore) {
        super(approved,autore);
    }

    @Override
    public String get() {
        return "Questa è una descrizione, creata da "+super.getAuthor()+", con id: "+super.getId();
    }
}
