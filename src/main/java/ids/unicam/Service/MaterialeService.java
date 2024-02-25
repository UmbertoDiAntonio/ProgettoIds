package ids.unicam.Service;

import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;

import java.util.List;
import java.util.Optional;

public interface MaterialeService {
    void approvaMateriale(MaterialeGenerico materialeGenerico, Stato stato);
    List<MaterialeGenerico> getAll();
    Optional<MaterialeGenerico> getById(int id);

    void deleteById(int id);

    Stato getStato(MaterialeGenerico foto);
}
