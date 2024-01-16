package ids.unicam.models.contenuti.POIFactory;

import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;
import org.jetbrains.annotations.Nullable;

import java.time.LocalDate;

public class Attivita extends PuntoInteresse {
    private LocalDate orario;
    public Attivita(String nome, Punto pt,LocalDate orario) {
        super(nome, pt);
        this.orario=orario;
    }

    public Attivita(String nome, Punto punto) {
        super(nome,punto);
    }

    public @Nullable LocalDate getOrario() {
        return orario;
    }

    @Override
    public void mostraDettagli() {

    }

    @Override
    public void getGeneralInfo() {

    }
}
