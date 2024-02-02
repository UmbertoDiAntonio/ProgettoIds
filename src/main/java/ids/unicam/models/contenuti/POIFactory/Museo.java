package ids.unicam.models.contenuti.POIFactory;

import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;
import org.jetbrains.annotations.Nullable;

import java.time.LocalDate;

public class Museo extends PuntoInteresse {
    private LocalDate data;
    public @Nullable LocalDate getData() {
        return data;
    }

    protected Museo(String nome, Punto punto, LocalDate data) {
        super(nome,punto);
        this.data=data;
    }

    protected Museo(String nome, Punto punto) {
        super(nome,punto);
    }

    @Override
    public String mostraInformazioniDettagliate() {
        //TODO
        return null;
    }

    @Override
    public String mostraInformazioniGeneriche() {
        //TODO
        return null;
    }


}
