package ids.unicam.Service;

import ids.unicam.models.Comune;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.Ruolo;
import ids.unicam.models.attori.TuristaAutenticato;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.GregorianCalendar;

public interface GestorePiattaformaService {
    TuristaAutenticato cambiaRuolo(Contributor contributor, @NotNull Ruolo ruolo);


    TuristaAutenticato registra(@Nullable Comune comune, Ruolo ruolo, String nome, String cognome, GregorianCalendar birthday, String password, String username);

}
