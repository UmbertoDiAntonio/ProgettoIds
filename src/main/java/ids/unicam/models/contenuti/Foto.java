package ids.unicam.models.contenuti;

import ids.unicam.models.attori.TuristaLoggato;

public class Foto extends Materiale{
    public Foto(TuristaLoggato autore) {
        super(autore);
    }


    @Override
    public String get() {
        return "Questa è una foto, creata da "+super.getAuthor().getName()+", con id: "+super.getId();
    }
}
