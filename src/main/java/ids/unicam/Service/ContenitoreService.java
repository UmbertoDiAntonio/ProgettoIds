package ids.unicam.Service;

import ids.unicam.models.contenuti.Contenitore;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.puntiInteresse.Tag;

import java.util.List;

public interface ContenitoreService {

    Contenitore salva(Contenitore contenitore);

    void aggiungiMateriale(Contenitore contenitore, MaterialeGenerico materialeGenerico);

    void rimuoviMateriale(Contenitore contenitore, MaterialeGenerico materialeGenerico);

    List<MaterialeGenerico> getMateriali (Contenitore contenitore);
}
