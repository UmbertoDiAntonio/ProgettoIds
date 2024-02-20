package ids.unicam.Service;

import ids.unicam.models.attori.Curatore;
import ids.unicam.models.contenuti.ContenutoGenerico;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import jakarta.transaction.Transactional;
import org.jetbrains.annotations.NotNull;

public interface CuratoreService {
    void valuta(Curatore curatore, @NotNull PuntoInteresse puntoInteresse, Stato stato);

    /**
     * Valuta un Materiale,
     * notifica i subscriber
     *
     * @param materialeGenerico il materiale che si vuole valutare
     * @param stato             approvato o non approvato
     */
    void valuta(Curatore curatore, MaterialeGenerico materialeGenerico, Stato stato);


    void elimina(Curatore curatore,PuntoInteresse puntoInteresse);

    void elimina(Curatore curatore,Itinerario itinerario);

    void elimina(Curatore curatore,Contest contest);

    void condividi(Curatore curatore, ContenutoGenerico contenutoGenerico);


    @Transactional
    void elimina(Curatore curatore, MaterialeGenerico materialeGenerico);

    void rimuoviTappa(Curatore curatore, Itinerario itinerario, PuntoInteresse tappa);
}
