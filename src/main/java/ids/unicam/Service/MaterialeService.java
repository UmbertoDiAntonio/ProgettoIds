package ids.unicam.Service;

import ids.unicam.models.DTO.MaterialeDTO;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import io.swagger.models.auth.In;

import java.util.List;
import java.util.Optional;

public interface MaterialeService {
    void approvaMateriale(MaterialeGenerico materialeGenerico, Stato stato);
    List<MaterialeGenerico> getAll();
    Optional<MaterialeGenerico> getById(int id);
    MaterialeGenerico update(MaterialeDTO materialeDTO, int id);
    void deleteById(int id);

}
