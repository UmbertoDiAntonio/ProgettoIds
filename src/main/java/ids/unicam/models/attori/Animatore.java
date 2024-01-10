package ids.unicam.models.attori;

import ids.unicam.controller.ContestController;
import ids.unicam.models.Comune;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Materiale;

import java.util.HashSet;
import java.util.Set;

public class Animatore extends Contributor {
    ContestController contestController;
    public Animatore(Comune comune,ContestController controller) {
        super(comune);
        this.contestController=controller;
    }


    public void creaContest(boolean open,String obiettivo){
        contestController.creaContest(open,obiettivo,this);
    }
    public void approva(Materiale materiale){
        contestController.getWaitingMaterials().remove(materiale);
        materiale.setApproved(true);
        materiale.getOwner().addMateriale(materiale);

    }

}
