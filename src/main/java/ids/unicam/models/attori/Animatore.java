package ids.unicam.models.attori;

import ids.unicam.models.Invito;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Materiale;

public class Animatore extends Contributor {


    public Animatore(Contributor contributor) {
        super(contributor.getComune(), contributor);
    }

    public void creaContest(String nome, String obiettivo, boolean open) {
        getComune().getContestController().creaContest(nome, open, obiettivo, this);
    }

    public void approva(Materiale materiale) {
        getComune().getContestController().getWaitingMaterials().remove(materiale);
        materiale.setApproved(true);
        materiale.getOwner().addMateriale(materiale);
    }

    public void invita(Contest contest, TuristaLoggato turistaLoggato){
        Invito invito = new Invito(contest, turistaLoggato);
        contest.getContestController().invita(invito);
        //turistaLoggato.getInvitiRicevuti().add(contest.getInviti().getFirst());
    }
}
