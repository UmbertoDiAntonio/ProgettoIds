package ids.unicam.Service;

import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;

import java.util.List;

public interface ContestService {
    List<TuristaAutenticato> getPartecipanti(Contest contest);

    Contest creaContest(Contest contest);

    List<Contest> getContestByPartecipante(TuristaAutenticato turistaAutenticato);

    List<Contest> getContestByCreatore(Animatore animatore);

    void approvaMateriale(MaterialeGenerico materialeGenerico, Stato stato);

    List<MaterialeGenerico> getMaterialiContest(Contest contest);

    void aggiungiMateriale(MaterialeGenerico materialeGenerico, Contest contest, TuristaAutenticato turistaAutenticato);

    void aggiungiPartecipante(Contest contest, TuristaAutenticato turistaAutenticato);
}