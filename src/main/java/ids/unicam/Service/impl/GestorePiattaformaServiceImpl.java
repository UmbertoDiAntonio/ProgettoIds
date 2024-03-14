package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.GestoreRepository;
import ids.unicam.Service.*;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.DTO.ContributorDTO;
import ids.unicam.models.DTO.TuristaAutenticatoDTO;
import ids.unicam.models.attori.*;
import ids.unicam.models.contenuti.RuoloRegistrazione;
import jakarta.transaction.Transactional;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Optional;

import static ids.unicam.Main.logger;

@Service
public class GestorePiattaformaServiceImpl implements GestorePiattaformaService {
    private final ContributorService contributorService;
    private final TuristaAutenticatoService turistaAutenticatoService;
    private final CuratoreService curatoreService;
    private final AnimatoreService animatoreServiceI;
    private final ContributorAutorizzatoService contributorAutorizzatoService;
    private final PoiService poiService;
    private final GestoreRepository repository;

    @Autowired
    public GestorePiattaformaServiceImpl(ContributorService contributorService, TuristaAutenticatoService turistaAutenticatoService, CuratoreService curatoreService, AnimatoreService animatoreServiceI, ContributorAutorizzatoService contributorAutorizzatoService, PoiService poiService, GestoreRepository repository) {
        this.contributorService = contributorService;
        this.turistaAutenticatoService = turistaAutenticatoService;
        this.curatoreService = curatoreService;
        this.animatoreServiceI = animatoreServiceI;
        this.contributorAutorizzatoService = contributorAutorizzatoService;
        this.poiService = poiService;
        this.repository = repository;
    }


    @Transactional
    @Override
    public @NotNull TuristaAutenticato cambiaRuolo(@NotNull String usernameGestore, @NotNull String usernameContributor, @NotNull Ruolo ruolo) throws IllegalArgumentException, ConnessioneFallitaException, UnsupportedOperationException {
        Optional<GestorePiattaforma> oGestore = findByUsername(usernameGestore);
        if (oGestore.isEmpty()) {
            logger.error("Devi essere il gestore della Piattaforma per cambiare ruoli");
            throw new IllegalArgumentException("Devi essere il gestore della Piattaforma per cambiare ruoli");
        }
        Optional<Contributor> oContributor = contributorService.getByUsername(usernameContributor);
        if (oContributor.isEmpty()) {
            logger.warn("username non valido");
            throw new IllegalArgumentException("username non valido");
        }
        Contributor contributor = oContributor.get();


        rimuoviVecchioRuolo(contributor);
        TuristaAutenticatoDTO turistaAutenticatoDTO = new TuristaAutenticatoDTO(contributor.getNome(), contributor.getCognome(), contributor.getDataNascita(), contributor.getPassword(), contributor.getUsername());
        Contributor modificato = switch (ruolo) {
            case TURISTA -> {
                logger.error("Non puoi tornare un turista");
                throw new UnsupportedOperationException("Non puoi tornare un turista");
            }
            case CURATORE ->
                    curatoreService.save(new Curatore(new ContributorDTO(contributor.getComune(), turistaAutenticatoDTO)));
            case ANIMATORE ->
                    animatoreServiceI.save(new Animatore(new ContributorDTO(contributor.getComune(), turistaAutenticatoDTO)));
            case CONTRIBUTOR_AUTORIZZATO ->
                    contributorAutorizzatoService.save(new ContributorAutorizzato(new ContributorDTO(contributor.getComune(), turistaAutenticatoDTO)));
            case CONTRIBUTOR ->
                    contributorService.save(new Contributor(new ContributorDTO(contributor.getComune(), turistaAutenticatoDTO)));
        };

        poiService.findAll().stream()
                .filter(puntoInteresse -> puntoInteresse.getCreatore() != null && puntoInteresse.getCreatore().getUsername().equals(contributor.getUsername()))
                .forEach(puntoInteresse -> {
                    puntoInteresse.setCreatore(modificato);
                    poiService.save(puntoInteresse);
                });

        return modificato;
    }

    /**
     * Rimuove il vecchio ruolo all'utente
     *
     * @param contributor l'utente a cui rimuovere il vecchio ruolo
     */
    private void rimuoviVecchioRuolo(@NotNull Contributor contributor) {
        switch (contributor) {
            case Curatore curatore -> curatoreService.deleteByUsername(curatore.getUsername());
            case ContributorAutorizzato contributorAutorizzato ->
                    contributorAutorizzatoService.deleteByUsername(contributorAutorizzato.getUsername());
            case Animatore animatore -> animatoreServiceI.deleteByUsername(animatore.getUsername());
            case Contributor contributor1 -> contributorService.deleteByUsername(contributor1.getUsername());
        }
    }

    /**
     * Verifica se i parametri per registrarsi sono validi
     *
     * @param turistaDTO il turista DTO
     * @throws IllegalArgumentException se uno dei parametri non risulta corretto
     */
    private void validaCredenziali(@NotNull TuristaAutenticatoDTO turistaDTO) throws IllegalArgumentException {
        if (!turistaDTO.getPassword().matches("^(?=.*[A-Z])(?=.*[@#$%^&+=])(?=.*[0-9])(?=.*[a-zA-Z]).{6,}$")) {
            logger.error("Password non valida, deve essere lunga almeno 6 caratteri, di cui almeno 1 numero, 1 maiuscola e un carattere speciale");
            throw new IllegalArgumentException("Password non valida, deve essere lunga almeno 6 caratteri, di cui almeno 1 numero, 1 maiuscola e un carattere speciale");
        }
        if (!turistaDTO.getUsername().matches("^.{5,}$")) {
            logger.error("Username non valido, deve essere lungo almeno 5 caratteri");
            throw new IllegalArgumentException("Username non valido, deve essere lungo almeno 5 caratteri");
        }
        if (!turistaAutenticatoService.isUsernameUnique(turistaDTO.getUsername())) {
            logger.error("Username già esistente");
            throw new IllegalArgumentException("Username già esistente");
        }
    }


    @Transactional
    public @NotNull TuristaAutenticato registra(@NotNull ContributorDTO contributorDTO, @NotNull RuoloRegistrazione ruolo) throws ConnessioneFallitaException, IllegalArgumentException {
        if (ruolo != RuoloRegistrazione.TURISTA && contributorDTO.getComune() == null) {
            logger.error("Il comune non puo' essere nullo, registrazione >= Contributor");
            throw new IllegalArgumentException("Il comune non puo' essere nullo, registrazione >= Contributor");
        }
        validaCredenziali(contributorDTO.getTuristaDTO());

        return switch (ruolo) {
            case TURISTA -> {
                TuristaAutenticato nuovoTurista = new TuristaAutenticato(contributorDTO.getTuristaDTO());
                yield turistaAutenticatoService.save(nuovoTurista);
            }
            case CONTRIBUTOR -> {
                Contributor contributor = new Contributor(contributorDTO);
                yield contributorService.save(contributor);
            }
        };
    }


    @Override
    public void creaGestore(@NotNull String username, @NotNull String password) {
        GestorePiattaforma gestorePiattaforma = GestorePiattaforma.getInstance(username, password);
        repository.save(gestorePiattaforma);
    }

    @Override
    public @NotNull Optional<GestorePiattaforma> findByUsername(@NotNull String username) {
        return repository.findById(username);
    }
}
