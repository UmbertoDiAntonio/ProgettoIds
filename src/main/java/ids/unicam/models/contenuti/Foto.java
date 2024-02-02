package ids.unicam.models.contenuti;

import ids.unicam.models.attori.TuristaAutenticato;
import jakarta.persistence.Entity;

@Entity
public class Foto extends Materiale{
    public Foto(TuristaAutenticato autore) {
        super(autore);
    }

    public Foto() {

    }


    @Override
    public String get() {
        return "Questa è una foto, creata da "+super.getCreatore().getNome()+", con id: "+super.getId();
    }
}
