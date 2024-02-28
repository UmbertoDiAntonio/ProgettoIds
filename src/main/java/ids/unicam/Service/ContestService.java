package ids.unicam.Service;

import ids.unicam.exception.ContestException;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;

import java.util.List;
import java.util.Optional;

public interface ContestService {
    Contest aggiungiMateriale(String usernameTurista, Integer idContest, MaterialeGenerico materialeGenerico) throws ContestException, FuoriComuneException;

    List<TuristaAutenticato> getPartecipanti(Contest contest);

    Contest creaContest(Contest contest);

    List<Contest> getContestByPartecipante(TuristaAutenticato turistaAutenticato);

    List<Contest> getContestByCreatore(Animatore animatore);

    List<MaterialeGenerico> getMaterialiContest(Contest contest);


    void aggiungiPartecipante(Contest contest, TuristaAutenticato turistaAutenticato) ;
    void rimuoviPartecipante(Contest contest, TuristaAutenticato turistaAutenticato);

    void setVincitoreContest(Contest contest,  MaterialeGenerico materialeGenerico);

    void terminaContest(Contest contest,Integer idMaterialeVincitore) throws ContestException;

    List<Contest>findAll();

    Optional<Contest> findById(int id);

    void deleteById(int id);

}
