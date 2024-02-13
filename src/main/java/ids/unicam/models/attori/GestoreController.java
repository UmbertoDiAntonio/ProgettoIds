package ids.unicam.models.attori;

import ids.unicam.Comune;
import ids.unicam.exception.RegistrazioneException;
import ids.unicam.models.Service.ContributorService;
import ids.unicam.models.Service.TuristaAutenticatoService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.GregorianCalendar;

@Component
public class GestoreController {

    private final TuristaAutenticatoService turistaAutenticatoService;
    private final ContributorService contributorService;


    @Autowired
    public GestoreController(TuristaAutenticatoService turistaAutenticatoService, ContributorService contributorService) {
        this.turistaAutenticatoService = turistaAutenticatoService;
        this.contributorService = contributorService;

    }




}
