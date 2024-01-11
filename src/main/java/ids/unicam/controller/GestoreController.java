package ids.unicam.controller;

import ids.unicam.models.Comune;
import ids.unicam.models.Gradi;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.Curatore;

import java.util.HashMap;

public class GestoreController {


    public static void upgrade(Comune comune,Contributor contributor,Gradi grado){
        switch (grado){
            case Curatore -> {
                comune.getCuratori().add(new Curatore(comune,contributor));
                comune.getContributors().remove(contributor);
            }
            case Animatore -> {

            }
            case ContributorTrusted ->{}
        }

    }


}
