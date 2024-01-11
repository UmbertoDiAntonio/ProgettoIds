package ids.unicam.models;

import ids.unicam.controller.UtentiController;
import ids.unicam.models.attori.*;

public enum Gradi {
    Contributor,
    ContributorTrusted,
    Animatore,
    Curatore;
/*
    public ids.unicam.models.attori.Contributor crea(Contributor contributor){
        return switch (this){
            case Contributor -> contributor;//TODO
            case ContributorTrusted -> new ContributorTrusted(contributor);
            case Animatore -> new Animatore(contributor);
            case Curatore -> new Curatore(contributor);
        };
    }

 */
}
