package ids.unicam.boundary;

import ids.unicam.Comune;
import ids.unicam.models.Ruolo;
import ids.unicam.models.Service.GestorePiattaformaService;
import ids.unicam.models.Service.TuristaAutenticatoService;
import ids.unicam.models.Service.TuristaService;

import java.util.GregorianCalendar;

public class MenuTurista {

    private TuristaAutenticatoService turistaAutenticatoService;
    private TuristaService turistaService;

    private GestorePiattaformaService gestorePiattaformaService;

    public void registra(Comune comune, int tipo, String nome, String cognome, GregorianCalendar birthday, String password, String username){
        gestorePiattaformaService.registra(null, Ruolo.TURISTA, nome, cognome, birthday, password, username);
    }
}
