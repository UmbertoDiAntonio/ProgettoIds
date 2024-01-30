package ids.unicam.models.contenuti;

import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.TuristaLoggato;

public class Foto extends Materiale{
    public Foto(boolean approved, TuristaLoggato autore) {
        super(approved,autore);
    }

    /**
     *
     * @return
     */
    @Override
    public String get() {
        return "Questa è una foto, creata da "+super.getAuthor()+", con id: "+super.getId();
    }
}
