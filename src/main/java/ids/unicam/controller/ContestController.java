package ids.unicam.controller;

import ids.unicam.Exception.NotInContestException;
import ids.unicam.models.Invito;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaLoggato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Materiale;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.HashSet;

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
    public @NotNull ArrayList<Contest> getContestByTourist(String idTurista) {
        ArrayList<Contest> result = new ArrayList<>();
        contests.stream().filter(contest -> contest.
                getPartecipanti().
                stream().anyMatch(turistaLoggato -> turistaLoggato.getId().equals(idTurista))
        ).forEach(result::add);
        return result;
    }

    /**
     *
     * @param idAutore l'id dell'Animatore
     * @return tutti i contest che ha creato
     */
    public @NotNull ArrayList<Contest> getContestByAuthor(long idAutore) {
        ArrayList<Contest> result = new ArrayList<>();
        contests.stream().filter(contest1 -> Long.parseLong(contest1.getAuthor().getId()) == idAutore).forEach(result::add);
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
     * @param turista il turista che sta caricando il materiale
     * @param materiale il materiale da caricare
     * @param contest il contest su cui si vuole aggiungere
     *
     * @throws NotInContestException se si cerca di caricare materiale su un contest senza essere entrati
     */
    public void aggiungiMateriale(TuristaLoggato turista,Materiale materiale,Contest contest){
            materiale.setApproved(false);
            contest.getMaterialiContest()
                    .computeIfAbsent(turista, k -> new HashSet<>())
                    .add(materiale);

    }
    /**
     * Imposta un materiale del contest come approvato e lo rimuove dalla lista dei materiali in attesa di approvazione
     * @param materiale il Materiale da approvare
     */
    public void approvaMateriale(Materiale materiale){//TODO forse va cambiato
        if(materiale.isApproved())
            return;
        materiale.setApproved(true);
    }

    public void invita(Invito invito) { //TODO perchè gli passiamo invito?
        invito.getInvitato().getInvitiRicevuti().add(invito); //TODO questo lo potrebbe dover fare utenti controller
    }

}
