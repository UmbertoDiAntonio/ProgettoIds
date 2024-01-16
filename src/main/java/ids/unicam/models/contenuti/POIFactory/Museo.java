package ids.unicam.models.contenuti.POIFactory;

import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;
import org.jetbrains.annotations.Nullable;

import java.time.LocalDate;

public class Museo extends PuntoInteresse {
    private LocalDate data;
    public Museo(String nome, Punto punto, LocalDate data) {
        super(nome,punto);
        this.data=data;
    }

    public @Nullable LocalDate getData() {
        return data;
    }

    public Museo(String nome, Punto punto) {
        super(nome,punto);
    }

    @Override
    public void mostraDettagli() {
        //todo
    }

    @Override
    public void getGeneralInfo() {
        //todo
    }


}
