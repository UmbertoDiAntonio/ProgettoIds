package ids.unicam.models.contenuti;

import ids.unicam.models.attori.TuristaAutenticato;
import jakarta.persistence.DiscriminatorValue;
import jakarta.persistence.Entity;

@Entity
@DiscriminatorValue("foto")
public class Foto extends MaterialeGenerico {
    public Foto(TuristaAutenticato autore) {
        super(autore);
    }

    public Foto() {    }


    @Override
    public String get() {
        return "Foto: "+getId()+", creata da "+ getCreatore().getNome();
    }
}
