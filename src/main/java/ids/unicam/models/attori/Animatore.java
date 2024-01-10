package ids.unicam.models.attori;

import ids.unicam.models.Comune;
import ids.unicam.models.contenuti.Contest;

import java.util.HashSet;
import java.util.Set;

public class Animatore extends Contributor {
    Set<Contest> contestCreati =new HashSet<>();

    public Animatore(Comune comune) {
        super(comune);
    }

    public void creaContest(){

    }

}
