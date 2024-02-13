package ids.unicam.models.Service;

import ids.unicam.Comune;
import ids.unicam.exception.RegistrazioneException;
import ids.unicam.models.Ruolo;
import ids.unicam.models.attori.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.GregorianCalendar;
import java.util.List;

@Service
public class GestorePiattaformaService {
    private final AnimatoreService animatoreService;
    private final ContributorService contributorService;
    private final ContributorAutorizzatoService contributorAutorizzatoService;
    private final CuratoreService curatoreService;
    private final TuristaAutenticatoService turistaAutenticatoService;

    @Autowired
    public GestorePiattaformaService(AnimatoreService animatoreService,
                                     ContributorService contributorService,
                                     ContributorAutorizzatoService contributorAutorizzatoService,
                                     CuratoreService curatoreService, TuristaAutenticatoService turistaAutenticatoService) {
        this.animatoreService = animatoreService;
        this.contributorService = contributorService;
        this.contributorAutorizzatoService = contributorAutorizzatoService;
        this.curatoreService = curatoreService;
        this.turistaAutenticatoService = turistaAutenticatoService;
    }


    public void promuovi(Contributor contributor, Ruolo ruolo){
        cambiaRuolo(contributor,ruolo);
    }
    /**
     * Modifica il ruolo di un contributor all'interno del comune
     *
     * @param contributor il contributor a cui cambiare ruolo
     * @param ruolo       il nuovo ruolo
     */
    private void cambiaRuolo(Contributor contributor, @NotNull Ruolo ruolo) {
        Comune comune = contributor.getComune();
        switch (ruolo) {
            case Curatore -> {
                curatoreService.save(new Curatore(contributor));
                rimuoviVecchioRuolo(contributor);
            }
            case Animatore -> {
                animatoreService.save(new Animatore(contributor));
                rimuoviVecchioRuolo(contributor);
            }
            case ContributorTrusted -> {
                contributorAutorizzatoService.save(new ContributorAutorizzato(contributor));
                rimuoviVecchioRuolo(contributor);
            }
            default -> {
                contributorService.save(new Contributor(comune, contributor));
                rimuoviVecchioRuolo(contributor);
            }
        }

    }

    private void rimuoviVecchioRuolo(@NotNull Contributor contributor) {
        switch (contributor) {
            case Curatore curatore -> curatoreService.deleteById(curatore.getId());
            case ContributorAutorizzato contributorAutorizzato ->
                    contributorAutorizzatoService.deleteById(contributorAutorizzato.getId());
            case Animatore animatore -> animatoreService.deleteById(animatore.getId());
            case Contributor contributor1 -> {
                contributorService.deleteById(contributor1.getId());
            }
        }
    }

    public List<TuristaAutenticato> getTuristi(){
        return turistaAutenticatoService.findAll();
    }


    public void eliminaTurista(TuristaAutenticato turistaAutenticato) {
        turistaAutenticatoService.deleteById(turistaAutenticato.getId());
    }

    public void eliminaListaTuristi() {
        turistaAutenticatoService.deleteAll();
    }

    public @Nullable TuristaAutenticato cercaTurista(int id) {
        return turistaAutenticatoService.findById(id).orElse(null);
    }


    public void registra(Comune comune, int tipo, String nome, String cognome, GregorianCalendar birthday, String password, String username) {
        switch (tipo) {
            case 0:
                registraTurista(nome, cognome, birthday, password, username);
                break;
            case 1:
                registraContributor(comune, nome, cognome, birthday, password, username);
                break;
            default:
                throw new RegistrazioneException("Il tipo di registrazione richiesta non è valido");
        }
    }

    public TuristaAutenticato registraTurista(String nome, String cognome, GregorianCalendar birthday, String password, String username) {
        TuristaAutenticato nuovoTurista = new TuristaAutenticato(nome, cognome, birthday, password, username);
        return turistaAutenticatoService.save(nuovoTurista);
    }

    public Contributor registraContributor(Comune comune, String nome, String cognome, GregorianCalendar birthday, String password, String username) {
        Contributor contributor = new Contributor(comune, nome, cognome, birthday, password, username);
        return contributorService.save(contributor);
    }


}
