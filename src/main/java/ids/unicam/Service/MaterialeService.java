package ids.unicam.Service;

import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.materiali.TipologiaMateriale;

import java.util.List;
import java.util.Optional;

public interface MaterialeService {
    MaterialeGenerico save(MaterialeGenerico materialeGenerico);

    /**
     * Ottieni la lista con tutti i materiali presenti nella piattaforma
     *
     * @return la lista di tutti i materiali trovati
     */
    List<MaterialeGenerico> getAll();

    /**
     * Ottieni, se esiste, il materiale con l'id cercato
     *
     * @param id l'id da cercare
     * @return il materiale con l'id cercato, Optional.Empty altrimenti
     */
    Optional<MaterialeGenerico> getById(int id);

    /**
     * Elimina il materiale con l'id indicato
     *
     * @param id l'id del materiale da eliminare
     */
    void deleteById(int id);

    /**
     * Crea un nuovo materiale
     *
     * @param fileMateriale      il path del file a cui è associato il materiale
     * @param tipologiaMateriale la tipologia di materiale
     * @param creatore           l'utente che ha creato il materiale
     * @return il materiale appena creato
     */
    MaterialeGenerico crea(String fileMateriale, TipologiaMateriale tipologiaMateriale, TuristaAutenticato creatore);

    /**
     * Ottieni la codifica in base 64 del materiale con l'id selezionato
     *
     * @param id l'id del materiale
     * @return la stringa contenente il materiale codificato in base 64
     */
    String getBase64ById(int id);

    /**
     * Ritorna lo stato del materiale
     *
     * @param idMateriale l'id del materiale di cui cercare lo stato
     * @return lo stato del materiale, come true, false o null se in attesa
     */
    Optional<Stato> getStato(int idMateriale);
}
