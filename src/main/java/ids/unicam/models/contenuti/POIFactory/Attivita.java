package ids.unicam.models.contenuti.POIFactory;

import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;
import org.jetbrains.annotations.Nullable;

import java.time.LocalDate;

public class Attivita extends PuntoInteresse {
    private LocalDate orario;
    public @Nullable LocalDate getOrario() {
        return orario;
    }
    protected Attivita(String nome, Punto pt,LocalDate orario) {
        super(nome, pt);
        this.orario=orario;
    }

    protected Attivita(String nome, Punto punto) {
        super(nome,punto);
    }

    @Override
    public String mostraInformazioniDettagliate() {
        //TODO
        return null;
    }

    @Override
    public String mostraInformazioniGeneriche() {
        return getNome() + " " + getOrario();
        //TODO
    }
}
