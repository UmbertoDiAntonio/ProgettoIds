package ids.unicam.models.contenuti;

import ids.unicam.models.attori.TuristaAutenticato;
import jakarta.persistence.Entity;

@Entity
public class Foto extends MaterialeGenerico {
    public Foto(TuristaAutenticato autore,PuntoInteresse puntoInteresse) {
        super(autore,puntoInteresse);
    }
    public Foto(TuristaAutenticato autore,Contest contest) {
        super(autore,contest);
    }

    public Foto() {    }


    @Override
    public String get() {
        return "Foto: "+getId()+", creata da "+ getCreatore().getNome();
    }
}
