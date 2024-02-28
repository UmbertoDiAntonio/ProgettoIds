package ids.unicam.Service;

import ids.unicam.models.contenuti.Contenitore;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;

import java.util.Set;

public interface ContenitoreService {


    void aggiungiMateriale(Contenitore contenitore, MaterialeGenerico materialeGenerico);

    void rimuoviMateriale(Contenitore contenitore, MaterialeGenerico materialeGenerico);

    Set<MaterialeGenerico> getMateriali (Contenitore contenitore);
}
