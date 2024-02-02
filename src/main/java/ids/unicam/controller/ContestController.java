package ids.unicam.controller;

import ids.unicam.Exception.ContestException;
import ids.unicam.models.Invito;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.Status;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;


public class ContestController {
    private final ArrayList<Contest> contests = new ArrayList<>();
    public ArrayList<Contest> getContests() {
        return contests;
    }
    /**
     *
     * @param idTurista l'id del Turista
     * @return tutti i contest di cui è membro
     */
    public @NotNull ArrayList<Contest> getContestDelTurista(long idTurista) {
        ArrayList<Contest> result = new ArrayList<>();
        contests.stream().filter(contest -> contest.
                getPartecipanti().
                stream().anyMatch(turistaLoggato -> turistaLoggato.getId()==(idTurista))
        ).forEach(result::add);
        return result;
    }

    /**
     *
     * @param idAutore l' id dell' Animatore
     * @return tutti i contest che ha creato
     */
    public @NotNull ArrayList<Contest> getContestDaAutore(long idAutore) {
        ArrayList<Contest> result = new ArrayList<>();
        contests.stream().filter(contest1 -> contest1.getCreatore().getId() == idAutore).forEach(result::add);
        return result;
    }

    /**
     * Crea e aggiungi un contest all'elenco dei contest
     * @param nome nome del contest
     * @param open contest aperto o su invito
     * @param obiettivo obiettivo del contest
     * @param animatore animatore che crea il contest
     * @return il contest appena creato
     */
    public Contest creaContest(String nome, boolean open, String obiettivo, Animatore animatore) {
        Contest contest = new Contest(nome, open, this,obiettivo, animatore);
        contests.add(contest);
        return contest;
    }


    /**
     * Aggiungi un materiale non approvato al contest, se i
     * @param materiale il materiale da caricare
     * @param contest il contest su cui si vuole aggiungere
     *
     * @throws ContestException se si cerca di caricare materiale su un contest senza essere entrati
     */
    public void aggiungiMateriale(Materiale materiale,Contest contest){
            materiale.setStato(Status.NOT_APPROVED);
            contest.getMaterialiContest().add(materiale);

    }
    /**
     * Imposta un materiale del contest come approvato
     * @param materiale il Materiale da approvare
     */
    public void approvaMateriale(Materiale materiale){
        if(materiale.getStato().getApprovato())
            return;
        materiale.setStato(Status.APPROVED);
    }

    public void generaInvito(TuristaAutenticato turistaAutenticato, Contest contest) {
        turistaAutenticato.getInvitiRicevuti().add(new Invito(contest, turistaAutenticato));
    }

    public void eliminaContest(Contest contest) {
        contests.remove(contest);
    }

}
