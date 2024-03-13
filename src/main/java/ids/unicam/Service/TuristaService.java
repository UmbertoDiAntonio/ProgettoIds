package ids.unicam.Service;

import ids.unicam.models.contenuti.Taggable;

import java.util.List;

public interface TuristaService {
    /**
     * Trova tutti i contenuti della piattaforma che hanno il tag indicato
     *
     * @param tag il tag da cercare
     * @return la lista dei contenuti trovati
     */
    List<Taggable> findByTag(String tag);

    void report(int idPuntoInteresse, String msg);

}
