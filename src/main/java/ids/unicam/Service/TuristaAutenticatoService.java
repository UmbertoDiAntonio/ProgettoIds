package ids.unicam.Service;

import ids.unicam.models.DTO.RichiestaCreazioneInvitoDTO;
import ids.unicam.models.DTO.RichiestaCreazioneTuristaDTO;
import ids.unicam.models.Invito;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;

import java.util.List;
import java.util.Optional;

public interface TuristaAutenticatoService {

    void accettaInvitoContest(RichiestaCreazioneTuristaDTO turistaDTO, RichiestaCreazioneInvitoDTO invitoDTO);

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

    List<TuristaAutenticato> getAll();

    Optional<TuristaAutenticato> getById(String username);

    void deleteById(String username);

    TuristaAutenticato update(RichiestaCreazioneTuristaDTO turistaDTO, String username);
}
