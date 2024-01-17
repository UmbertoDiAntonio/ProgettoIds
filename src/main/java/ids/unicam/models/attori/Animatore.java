package ids.unicam.models.attori;

import ids.unicam.models.Invito;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Materiale;

public class Animatore extends Contributor {


    public Animatore(Contributor contributor) {
        super(contributor.getComune(), contributor);
    }

    public void creaContest(String obiettivo, boolean open) {
        getComune().getContestController().creaContest(open, obiettivo, this);
    }

    public void approva(Materiale materiale) {
        getComune().getContestController().getWaitingMaterials().remove(materiale);
        materiale.setApproved(true);
        materiale.getOwner().addMateriale(materiale);
    }

    public void invita(Contest contest, TuristaLoggato turistaLoggato){
        new Invito(contest, turistaLoggato.getId());
    }
}
