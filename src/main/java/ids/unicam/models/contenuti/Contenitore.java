package ids.unicam.models.contenuti;

import ids.unicam.models.contenuti.materiali.MaterialeGenerico;

import java.util.List;

public interface Contenitore {
    List<MaterialeGenerico> getMateriali();
    void addMateriale(MaterialeGenerico materialeGenerico);
    void rimuoviMateriale(MaterialeGenerico materialeGenerico);
}
