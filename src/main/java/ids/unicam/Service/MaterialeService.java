package ids.unicam.Service;

import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;

public interface MaterialeService {
    void approvaMateriale(MaterialeGenerico materialeGenerico, Stato stato);
}
