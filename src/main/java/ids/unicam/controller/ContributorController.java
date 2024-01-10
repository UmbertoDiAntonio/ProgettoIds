package ids.unicam.controller;

import ids.unicam.models.Gradi;
import ids.unicam.models.attori.Contributor;

import java.util.HashMap;

public class ContributorController {
    private static final HashMap<Contributor, Gradi> contributori =new HashMap<>();

    public static void upgrade(Contributor contributor,Gradi grado){
        contributori.put(contributor,grado);
    }
}
