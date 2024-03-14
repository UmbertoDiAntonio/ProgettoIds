package ids.unicam.Service;

import ids.unicam.models.contenuti.Taggable;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public interface TuristaService {
    /**
     * Trova tutti i contenuti della piattaforma che hanno il tag indicato
     *
     * @param tag il tag da cercare
     * @return la lista dei contenuti trovati
     */
    @NotNull List<Taggable> findByTag(@NotNull String tag);

    void report(int idPuntoInteresse, @NotNull String msg);

}
