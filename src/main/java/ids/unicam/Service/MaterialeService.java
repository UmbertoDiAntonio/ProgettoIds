package ids.unicam.Service;

import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contenitore;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.materiali.TipologiaMateriale;

import java.util.List;
import java.util.Optional;

public interface MaterialeService {
    void approvaMateriale(MaterialeGenerico materialeGenerico, Stato stato);
    List<MaterialeGenerico> getAll();
    Optional<MaterialeGenerico> getById(int id);

    void deleteById(int id);

    Stato getStato(MaterialeGenerico foto);


    MaterialeGenerico crea(String fileMateriale, TipologiaMateriale tipologiaMateriale, TuristaAutenticato creatore) throws IllegalArgumentException;

    String getBase64ById(Integer id);

    void aggiungiMateriale(Contenitore contenitore, MaterialeGenerico materialeGenerico);
}
