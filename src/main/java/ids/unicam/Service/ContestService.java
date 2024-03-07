package ids.unicam.Service;

import ids.unicam.exception.ContestException;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.Comune;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import jakarta.transaction.Transactional;

import java.util.List;
import java.util.Optional;

public interface ContestService {
    void aggiungiMateriale(String usernameTurista, int idContest, MaterialeGenerico materialeGenerico) throws ContestException, FuoriComuneException;

    List<TuristaAutenticato> getPartecipanti(Contest contest);

    Contest creaContest(String nomeContest, String obiettivo, Animatore creatore, boolean open);

    List<Taggable> findByTag(String tag);

    List<Contest> getContestByCreatore(Animatore animatore);

    List<MaterialeGenerico> getMaterialiContest(Contest contest);

    void aggiungiPartecipante(Contest contest, TuristaAutenticato turistaAutenticato) throws ContestException;

    void rimuoviPartecipante(Contest contest, TuristaAutenticato turistaAutenticato);

    void setVincitoreContest(Contest contest, MaterialeGenerico materialeGenerico) throws ContestException;

    void terminaContest(Contest contest) throws ContestException;

    List<Contest> findAll();

    Contest save(Contest contest);

    Optional<Contest> findById(int id);

    void deleteById(int id);

    Optional<Contest> getContestContainingMaterial(MaterialeGenerico materialeGenerico);

    @Transactional
    void checkIfIsExpired(Contest contest);

    List<Contest> getContestByComune(String nomeComune);
}
