package ids.unicam.models.attori;

import ids.unicam.Comune;
import ids.unicam.DataBase.GestoreDatabase;
import ids.unicam.exception.RegistrazioneException;
import ids.unicam.models.Ruolo;
import ids.unicam.models.Service.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.GregorianCalendar;
import java.util.List;

@Component
public class GestoreController {

    private final TuristaAutenticatoService turistaAutenticatoService;
    private final ContributorService contributorService;
    private final AnimatoreService animatoreService;
    private final CuratoreService curatoreService;
    private final ContributorAutorizzatoService contributorAutorizzatoService;


    @Autowired
    public GestoreController(GestoreDatabase gestoreDatabase, TuristaAutenticatoService turistaAutenticatoService, ContributorService contributorService, AnimatoreService animatoreService, CuratoreService curatoreService, ContributorAutorizzatoService contributorAutorizzatoService) {
        this.turistaAutenticatoService = turistaAutenticatoService;
        this.contributorService = contributorService;
        this.animatoreService = animatoreService;
        this.curatoreService = curatoreService;
        this.contributorAutorizzatoService = contributorAutorizzatoService;
    }

    /**
     * Modifica il ruolo di un contributor all'interno del comune
     *
     * @param contributor il contributor a cui cambiare ruolo
     * @param ruolo       il nuovo ruolo
     */
    public void cambiaRuolo(Contributor contributor, @NotNull Ruolo ruolo) {
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
        turistaAutenticatoService.save(nuovoTurista);
        return nuovoTurista;
    }

    public Contributor registraContributor(Comune comune, String nome, String cognome, GregorianCalendar birthday, String password, String username) {
        Contributor contributor = new Contributor(comune, nome, cognome, birthday, password, username);
        contributorService.save(contributor);
        return contributor;

        //TODO aggiungere al database
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

    public List<TuristaAutenticato> trovaTutti() {
        return turistaAutenticatoService.findAll();
    }

    public TuristaAutenticato prendiUltimoTurista() {
        return turistaAutenticatoService.getLast();
    }

    public TuristaAutenticato prendiPrimoTurista() {
        return turistaAutenticatoService.getFirst();
    }


}
