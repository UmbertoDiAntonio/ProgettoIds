package ids.unicam.models.contenuti;

import ids.unicam.models.attori.TuristaLoggato;
import jakarta.persistence.Entity;

@Entity
public class Foto extends Materiale{
    public Foto(TuristaLoggato autore) {
        super(autore);
    }

    public Foto() {

    }


    @Override
    public String get() {
        return "Questa è una foto, creata da "+super.getAuthor().getName()+", con id: "+super.getId();
    }
}
