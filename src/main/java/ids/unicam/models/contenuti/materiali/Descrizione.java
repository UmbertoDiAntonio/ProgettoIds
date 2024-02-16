package ids.unicam.models.contenuti.materiali;

import ids.unicam.models.users.TuristaAutenticato;
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
