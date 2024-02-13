package ids.unicam.models.contenuti;

import ids.unicam.models.attori.TuristaAutenticato;
import jakarta.persistence.DiscriminatorValue;
import jakarta.persistence.Entity;

@Entity
@DiscriminatorValue("descrizione")
public class Descrizione  extends MaterialeGenerico {
    public Descrizione(TuristaAutenticato autore, PuntoInteresse puntoInteresse) {
        super(autore,puntoInteresse);
    }
    public Descrizione(TuristaAutenticato autore, Contest contest) {
        super(autore,contest);
    }

    public Descrizione() {

    }

    @Override
    public String get() {
        return "Descrizione: "+getId()+", creata da "+ getCreatore().getNome();
    }
}
