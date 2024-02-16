package ids.unicam.models.Service;

import ids.unicam.Comune;
import ids.unicam.models.Ruolo;
import ids.unicam.models.attori.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.GregorianCalendar;

import static ids.unicam.Main.logger;

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
                                     CuratoreService curatoreService,
                                     TuristaAutenticatoService turistaAutenticatoService) {
        this.animatoreService = animatoreService;
        this.contributorService = contributorService;
        this.contributorAutorizzatoService = contributorAutorizzatoService;
        this.curatoreService = curatoreService;
        this.turistaAutenticatoService = turistaAutenticatoService;
    }


    public TuristaAutenticato promuovi(Contributor contributor, Ruolo ruolo) {
        return cambiaRuolo(contributor, ruolo);
    }

    /**
     * Modifica il ruolo di un contributor all'interno del comune
     *
     * @param contributor il contributor a cui cambiare ruolo
     * @param ruolo       il nuovo ruolo
     */
    private TuristaAutenticato cambiaRuolo(Contributor contributor, @NotNull Ruolo ruolo) {
        Comune comune = contributor.getComune();
        rimuoviVecchioRuolo(contributor);

        return switch (ruolo) {
            case TURISTA -> {
                logger.error("Non puoi tornare un turista");
                yield null;
            }
            case CURATORE -> curatoreService.save(new Curatore(contributor));
            case ANIMATORE -> animatoreService.save(new Animatore(contributor));
            case CONTRIBUTOR_AUTORIZZATO -> contributorAutorizzatoService.save(new ContributorAutorizzato(contributor));
            case CONTRIBUTOR -> contributorService.save(new Contributor(comune, contributor));
        };

    }

    private void rimuoviVecchioRuolo(@NotNull Contributor contributor) {
        switch (contributor) {
            case Curatore curatore -> curatoreService.deleteById(curatore.getId());
            case ContributorAutorizzato contributorAutorizzato ->
                    contributorAutorizzatoService.deleteById(contributorAutorizzato.getId());
            case Animatore animatore -> animatoreService.deleteById(animatore.getId());
            case Contributor contributor1 -> contributorService.deleteById(contributor1.getId());
        }
    }





    public TuristaAutenticato registra(@Nullable Comune comune, Ruolo ruolo, String nome, String cognome, GregorianCalendar birthday, String password, String username) {
        if (ruolo != Ruolo.TURISTA && comune == null) {
            logger.error("Il comune non puo' essere nullo, registrazione >= Contributor");
            throw new RuntimeException("Il comune non puo' essere nullo, registrazione >= Contributor");
        }
        if (!password.matches("^(?=.*[A-Z])(?=.*[@#$%^&+=])(?=.*[0-9])(?=.*[a-zA-Z]).{6,}$")) {
            logger.error("Password non valida");
            throw new IllegalArgumentException("Password non valida");
            //TODO messaggio da parte dell'interfaccia
        }
        if (!username.matches("^.{5,}$")) {
            logger.error("Username non valido");
            throw new IllegalArgumentException("Username non valido");
            //TODO messaggio da parte dell'interfaccia
        }
        if (!turistaAutenticatoService.isUsernameUnique(username)) {
            logger.error("Username già esistente");
            throw new IllegalArgumentException("Username già esistente");
            //TODO messaggio da parte dell'interfaccia
        }
        return switch (ruolo) {
            case TURISTA -> registraTurista(nome, cognome, birthday, password, username);
            case CONTRIBUTOR -> registraContributor(comune, nome, cognome, birthday, password, username);
            case CURATORE, ANIMATORE, CONTRIBUTOR_AUTORIZZATO -> {
                Contributor contributor = registraContributor(comune, nome, cognome, birthday, password, username);
                yield promuovi(contributor, ruolo);
            }
        };
    }

    private TuristaAutenticato registraTurista(String nome, String cognome, GregorianCalendar birthday, String password, String username) {
        TuristaAutenticato nuovoTurista = new TuristaAutenticato(nome, cognome, birthday, password, username);
        return turistaAutenticatoService.save(nuovoTurista);
    }

    private Contributor registraContributor(Comune comune, String nome, String cognome, GregorianCalendar birthday, String password, String username) {
        Contributor contributor = new Contributor(comune, nome, cognome, birthday, password, username);
        return contributorService.save(contributor);
    }


}
