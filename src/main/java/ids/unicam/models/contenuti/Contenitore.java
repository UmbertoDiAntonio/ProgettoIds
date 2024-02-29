package ids.unicam.models.contenuti;

import ids.unicam.models.contenuti.materiali.MaterialeGenerico;

import java.util.Set;

public interface Contenitore {
    Set<MaterialeGenerico> getMateriali();

    void addMateriale(MaterialeGenerico materialeGenerico);

    void rimuoviMateriale(MaterialeGenerico materialeGenerico);
}
