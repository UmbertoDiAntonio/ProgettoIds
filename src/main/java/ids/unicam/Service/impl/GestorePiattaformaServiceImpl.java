package ids.unicam.Service.impl;

import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.models.Comune;
import ids.unicam.models.attori.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.GregorianCalendar;

import static ids.unicam.Main.logger;

@Service
public class GestorePiattaformaServiceImpl implements GestorePiattaformaService {
    private final ContributorServiceImpl contributorServiceImpl;
    private final TuristaAutenticatoServiceImpl turistaAutenticatoServiceImpl;
    private final CuratoreServiceImpl curatoreServiceImpl;
    private final AnimatoreServiceImpl animatoreServiceImpl;
    private final ContributorAutorizzatoServiceImpl contributorAutorizzatoServiceImpl;

    @Autowired
    public GestorePiattaformaServiceImpl(ContributorServiceImpl contributorServiceImpl, TuristaAutenticatoServiceImpl turistaAutenticatoServiceImpl,
                                         CuratoreServiceImpl curatoreServiceImpl,AnimatoreServiceImpl animatoreServiceImpl, ContributorAutorizzatoServiceImpl contributorAutorizzatoServiceImpl) {
        this.contributorServiceImpl = contributorServiceImpl;
        this.turistaAutenticatoServiceImpl = turistaAutenticatoServiceImpl;
        this.animatoreServiceImpl = animatoreServiceImpl;
        this.curatoreServiceImpl = curatoreServiceImpl;
        this.contributorAutorizzatoServiceImpl = contributorAutorizzatoServiceImpl;
    }


    /**
     * Modifica il ruolo di un contributor all'interno del comune
     *
     * @param contributor il contributor a cui cambiare ruolo
     * @param ruolo       il nuovo ruolo
     */
    @Override
    public TuristaAutenticato cambiaRuolo(Contributor contributor, @NotNull Ruolo ruolo) {
        Comune comune = contributor.getComune();
        rimuoviVecchioRuolo(contributor);

        return switch (ruolo) {
            case TURISTA -> {
                logger.error("Non puoi tornare un turista");
                yield null;
            }
            case CURATORE -> curatoreServiceImpl.save(new Curatore(contributor));
            case ANIMATORE -> animatoreServiceImpl.save(new Animatore(contributor));
            case CONTRIBUTOR_AUTORIZZATO ->
                    contributorAutorizzatoServiceImpl.save(new ContributorAutorizzato(contributor));
            case CONTRIBUTOR -> contributorServiceImpl.save(new Contributor(comune, contributor));

        };

    }


    private void rimuoviVecchioRuolo(@NotNull Contributor contributor) {
        switch (contributor) {
            case Curatore curatore -> curatoreServiceImpl.deleteById(curatore.getId());
            case ContributorAutorizzato contributorAutorizzato ->
                    contributorAutorizzatoServiceImpl.deleteById(contributorAutorizzato.getId());
            case Animatore animatore -> animatoreServiceImpl.deleteById(animatore.getId());
            case Contributor contributor1 -> contributorServiceImpl.deleteById(contributor1.getId());
        }
    }



    @Override
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
        if (!turistaAutenticatoServiceImpl.isUsernameUnique(username)) {
            logger.error("Username già esistente");
            throw new IllegalArgumentException("Username già esistente");
            //TODO messaggio da parte dell'interfaccia
        }
        return switch (ruolo) {
            case TURISTA -> registraTurista(nome, cognome, birthday, password, username);
            case CONTRIBUTOR -> registraContributor(comune, nome, cognome, birthday, password, username);
            case CURATORE, ANIMATORE, CONTRIBUTOR_AUTORIZZATO -> {
                Contributor contributor = registraContributor(comune, nome, cognome, birthday, password, username);
                yield cambiaRuolo(contributor, ruolo);
            }
        };
    }

    private TuristaAutenticato registraTurista(String nome, String cognome, GregorianCalendar birthday, String password, String username) {
        TuristaAutenticato nuovoTurista = new TuristaAutenticato(nome, cognome, birthday, password, username);
        return turistaAutenticatoServiceImpl.save(nuovoTurista);
    }

    private Contributor registraContributor(Comune comune, String nome, String cognome, GregorianCalendar birthday, String password, String username) {
        Contributor contributor = new Contributor(comune, nome, cognome, birthday, password, username);
        return contributorServiceImpl.save(contributor);
    }


}
