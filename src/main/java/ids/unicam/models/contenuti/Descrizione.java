package ids.unicam.models.contenuti;

import ids.unicam.models.attori.TuristaAutenticato;
import jakarta.persistence.DiscriminatorValue;
import jakarta.persistence.Entity;

@Entity
@DiscriminatorValue("descrizione")
public class Descrizione  extends MaterialeGenerico {
    public Descrizione(TuristaAutenticato autore) {
        super(autore);
    }

    public Descrizione() {

    }

    @Override
    public String get() {
        return "Descrizione: "+getId()+", creata da "+ getCreatore().getNome();
    }
}
