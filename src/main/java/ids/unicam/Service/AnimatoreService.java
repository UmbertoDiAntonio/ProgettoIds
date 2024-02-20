package ids.unicam.Service;

import ids.unicam.models.Invito;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;

public interface AnimatoreService {
    Contest creaContest(Animatore animatore, String nomeContest, String obiettivo, boolean tipoContest);

    Invito invitaContest(Animatore animatore, Contest contest, TuristaAutenticato turistaAutenticato);

    boolean approvaMateriale(Animatore animatore, Contest contest, MaterialeGenerico materialeGenerico, Stato stato);

}
