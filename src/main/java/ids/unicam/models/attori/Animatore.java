package ids.unicam.models.attori;

import ids.unicam.models.Comune;
import ids.unicam.models.contenuti.Materiale;

public class Animatore extends Contributor {

    public Animatore(Comune comune) {
        super(comune);
    }


    public void creaContest(boolean open,String obiettivo){
        comune.getContestController().creaContest(open,obiettivo,this);
    }
    public void approva(Materiale materiale){
        comune.getContestController().getWaitingMaterials().remove(materiale);
        materiale.setApproved(true);
        materiale.getOwner().addMateriale(materiale);

    }

}
