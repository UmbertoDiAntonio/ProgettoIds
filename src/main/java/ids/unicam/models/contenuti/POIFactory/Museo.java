package ids.unicam.models.contenuti.POIFactory;

import ids.unicam.Comune;
import ids.unicam.models.Orario;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;
import org.jetbrains.annotations.Nullable;

public class Museo extends PuntoInteresse {
    private Orario orario;
    public @Nullable Orario getOrario() {
        return orario;
    }

    protected Museo(Comune comune,String nome, Punto punto, Orario orario) {
        super(comune,nome,punto);
        this.orario = orario;
    }

    protected Museo(Comune comune, String nome, Punto punto) {
        super(comune,nome,punto);
        new Orario();
    }

    @Override
    public String mostraInformazioniDettagliate() {
        //TODO
        return "";
    }

    @Override
    public String mostraInformazioniGeneriche() {
        //TODO
        return getNome()+" "+ getOrario();
    }


}
