package ids.unicam.controller;

import ids.unicam.models.Comune;
import ids.unicam.models.Gradi;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.ContributorTrusted;
import ids.unicam.models.attori.Curatore;

public class GestoreController {


    public static void upgrade(Comune comune, Contributor contributor, Gradi grado) {
        switch (grado) {
            case Curatore -> {
                comune.getCuratori().add(new Curatore(comune, contributor));
                removeOldRank(comune, contributor);
            }
            case Animatore -> {
                comune.getAnimatori().add(new Animatore(contributor));
                removeOldRank(comune, contributor);
            }
            case ContributorTrusted -> {
                removeOldRank(comune, contributor);
                comune.getContributors().add(new ContributorTrusted(comune,contributor));//TODO verifica

            }
            default -> {
                comune.getContributors().add(new Contributor(comune,contributor));//TODO verifica
                removeOldRank(comune, contributor);
            }
        }

    }

    private static void removeOldRank(Comune comune, Contributor contributor) {
        switch (contributor) {
            case Curatore curatore ->{ comune.getCuratori().remove(curatore);}
            case ContributorTrusted contributorTrusted -> {
                comune.getContributors().remove(contributorTrusted);//TODO verifica
            }
            case Animatore animatore -> {comune.getAnimatori().remove(animatore);}
            case Contributor contributor1 -> {
                comune.getContributors().remove(contributor1);
            }
            case null -> System.out.println("NULLL"); //TODO
        }
    }
}
