package ids.unicam.models.contenuti.materiali;

import ids.unicam.models.users.TuristaAutenticato;
import jakarta.persistence.DiscriminatorValue;
import jakarta.persistence.Entity;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Entity
@DiscriminatorValue("descrizione")
public class Descrizione  extends MaterialeGenerico {
    public Descrizione(TuristaAutenticato autore) {
        super(autore);
    }

    @Override
    public String get() {
        return "Descrizione: "+getId()+", creata da "+ getCreatore().getNome();
    }

    @Override
    public String toString() {
        return super.toString();
    }
}
