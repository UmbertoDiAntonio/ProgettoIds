package ids.unicam.controller;

import ids.unicam.models.Comune;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.ContributorTrusted;
import ids.unicam.models.attori.TuristaLoggato;

import java.util.ArrayList;
import java.util.Date;

public class UtentiController {
    private static long id = 0;

    public static String generateID() {
        return String.format("%04d", id++);
    }

    private final ArrayList<TuristaLoggato> turisti = new ArrayList<>();

    public ArrayList<TuristaLoggato> getTuristi() {
        return turisti;
    }


}
