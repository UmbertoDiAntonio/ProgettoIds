package ids.unicam.Service;

import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contenitore;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;

import java.util.List;

public interface MaterialeService {
    MaterialeGenerico salvaMateriale(MaterialeGenerico materialeGenerico);

    void approvaMateriale(MaterialeGenerico materialeGenerico, Stato stato);



}
