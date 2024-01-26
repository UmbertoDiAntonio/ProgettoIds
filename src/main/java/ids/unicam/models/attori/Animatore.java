package ids.unicam.models.attori;

import ids.unicam.models.Invito;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Materiale;

public class Animatore extends Contributor {
    protected Animatore(Contributor contributor) {
        super(contributor.getComune(), contributor);
    }

    /**
     * Invia al controller di contest associato al comune dell'animatore la richiesta di creare e aggiungere un contest
     * @param nome nome del contest da creare
     * @param obiettivo obiettivo del contest
     * @param open contest aperto o su invito
     * @return il contest generato
     */
    public Contest creaContest(String nome, String obiettivo, boolean open) {
        return getComune().getContestController().creaContest(nome, open, obiettivo, this);
    }

    /**
     * Invia al controller di contest associato al comune dell'animatore la richiesta di approvare il materiale
     * @param materiale il materiale da approvare
     */
    public void approva(Materiale materiale) { //TODO come capiamo se è andato a buon fine?
        getComune().getContestController().approvaMateriale(materiale);
    }

    /**
     * Crea un invito al contest per il turista, poi tramite il contest controller???? lo aggiungiamo all'utente
     * @param contest
     * @param turistaLoggato
     */
    public void invita(Contest contest, TuristaLoggato turistaLoggato){
        Invito invito = new Invito(contest, turistaLoggato); //TODO non mi piace
        contest.getContestController().invita(invito);
        //turistaLoggato.getInvitiRicevuti().add(contest.getInviti().getFirst());
    }
}
