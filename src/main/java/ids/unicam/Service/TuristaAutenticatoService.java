package ids.unicam.Service;

import ids.unicam.models.Invito;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;

import java.util.List;
import java.util.Optional;

public interface TuristaAutenticatoService {

    void accettaInvitoContest(TuristaAutenticato turistaAutenticato, Invito invito);

    void rimuoviPreferito(TuristaAutenticato turistaAutenticato, int id);

    void aggiungiPreferito(TuristaAutenticato turista, PuntoInteresse puntoInteresse);

    List<PuntoInteresse> findPreferiti(TuristaAutenticato turistaAutenticato);

    /**
     * Entra nel contest se è aperto
     *
     * @param contest il contest in cui si vuole entrare
     */
    void partecipaAlContest(Contest contest, TuristaAutenticato turistaAutenticato);


    Optional<TuristaAutenticato> findTuristaByUsername(String username);

    boolean verificaPassword(String password, String username);

    boolean isUsernameUnique(String username);
}
