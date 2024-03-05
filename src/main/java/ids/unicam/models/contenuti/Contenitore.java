package ids.unicam.models.contenuti;

import ids.unicam.models.contenuti.materiali.MaterialeGenerico;

import java.util.Set;

/**
 * Interfaccia usata dagli oggetti che possono contenere materiali, fornisce i metodi per la loro aggiunta, rimozione
 * e per ottenerli
 */
public interface Contenitore {
    /**
     * Restituisce l'elenco dei materiali del contenitore sotto forma di un Set<MaterialeGenerico>
     * @return l'elenco dei materiali del contenitore
     */
    Set<MaterialeGenerico> getMateriali();

    /**
     * Aggiungi un oggetto che estende MaterialeGenerico di qualunque tipo ai materiali del contenitore
     * @param materialeGenerico il materiale da aggiungere
     */
    void aggiungiMateriale(MaterialeGenerico materialeGenerico);

    /**
     * Rimuovi un MaterialeGenerico di qualunque tipo ai materiali del contenitore
     * @param materialeGenerico il materiale da rimuovere
     */
    void rimuoviMateriale(MaterialeGenerico materialeGenerico);
}
