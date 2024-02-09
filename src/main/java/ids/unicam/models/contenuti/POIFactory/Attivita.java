package ids.unicam.models.contenuti.POIFactory;

import ids.unicam.Comune;
import ids.unicam.models.Orario;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;
import jakarta.persistence.Embedded;
import jakarta.persistence.Entity;
import jakarta.persistence.Transient;
import org.jetbrains.annotations.Nullable;
@Entity
public class Attivita extends PuntoInteresse {
    @Embedded
    private Orario orario;

    protected Attivita(Comune comune, String nome, Punto pt, Orario orario) {
        super(comune,nome, pt);
        this.orario = orario;
    }

    protected Attivita(Comune  comune,String nome, Punto punto) {
        super(comune,nome, punto);
    }

    public Attivita() {

    }
    public @Nullable Orario getOrario() {
        return orario;
    }

    @Override
    public String mostraInformazioniDettagliate() {
        //TODO
        return "";
    }

    @Override
    public String mostraInformazioniGeneriche() {
        return getNome() + " " + getOrario();
        //TODO
    }
}
