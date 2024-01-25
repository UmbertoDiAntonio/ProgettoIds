package ids.unicam.models.contenuti.POIFactory;

import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;
import org.jetbrains.annotations.Nullable;

import java.time.LocalDate;

public class Attivita extends PuntoInteresse {
    private LocalDate orario;
    protected Attivita(String nome, Punto pt,LocalDate orario) {
        super(nome, pt);
        this.orario=orario;
    }

    protected Attivita(String nome, Punto punto) {
        super(nome,punto);
    }

    public @Nullable LocalDate getOrario() {
        return orario;
    }

    @Override
    public String mostraDettagli() {
        //TODO
        return null;
    }

    @Override
    public String getGeneralInfo() {
        return getNome() + " " + getOrario();
        //TODO
    }
}
