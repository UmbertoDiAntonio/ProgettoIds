package ids.unicam.Service.impl;

import ids.unicam.Service.*;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.DTO.ContributorDTO;
import ids.unicam.models.DTO.TuristaAutenticatoDTO;
import ids.unicam.models.attori.*;
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

    @Autowired
    public GestorePiattaformaServiceImpl(ContributorService contributorService, TuristaAutenticatoService turistaAutenticatoService, CuratoreService curatoreService, AnimatoreService animatoreServiceI, ContributorAutorizzatoService contributorAutorizzatoService, PoiService poiService) {
        this.contributorService = contributorService;
        this.turistaAutenticatoService = turistaAutenticatoService;
        this.curatoreService = curatoreService;
        this.animatoreServiceI = animatoreServiceI;
        this.contributorAutorizzatoService = contributorAutorizzatoService;
        this.poiService = poiService;
    }


    @Transactional
    @Override
    public TuristaAutenticato cambiaRuolo(String usernameContributor, @NotNull Ruolo ruolo) throws IllegalArgumentException, ConnessioneFallitaException, UnsupportedOperationException {
        Optional<TuristaAutenticato> oTurista = turistaAutenticatoService.getByUsername(usernameContributor);
        if (oTurista.isEmpty()) {
            logger.error("username non valido");
            throw new IllegalArgumentException("username non valido");
        }
        TuristaAutenticato turistaAutenticato = oTurista.get();

        if (!(turistaAutenticato instanceof Contributor contributor)) {
            logger.error("il turista non puo' cambiare ruolo");
            throw new UnsupportedOperationException("il turista non puo' cambiare ruolo");
        }
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


    private void rimuoviVecchioRuolo(@NotNull Contributor contributor) {
        switch (contributor) {
            case Curatore curatore -> curatoreService.deleteByUsername(curatore.getUsername());
            case ContributorAutorizzato contributorAutorizzato ->
                    contributorAutorizzatoService.deleteByUsername(contributorAutorizzato.getUsername());
            case Animatore animatore -> animatoreServiceI.deleteByUsername(animatore.getUsername());
            case Contributor contributor1 -> contributorService.deleteByUsername(contributor1.getUsername());
        }
    }

    private void validaCredenziali(TuristaAutenticatoDTO turistaDTO) throws IllegalArgumentException {
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

    public TuristaAutenticato registraTurista(TuristaAutenticatoDTO turistaDTO) throws IllegalArgumentException {
        validaCredenziali(turistaDTO);
        TuristaAutenticato nuovoTurista = new TuristaAutenticato(turistaDTO);
        return turistaAutenticatoService.save(nuovoTurista);
    }

    @Transactional
    public TuristaAutenticato registraContributor(ContributorDTO contributorDTO, Ruolo ruolo) throws ConnessioneFallitaException, RuntimeException {
        if (ruolo != Ruolo.TURISTA && contributorDTO.getComune() == null) {
            logger.error("Il comune non puo' essere nullo, registrazione >= Contributor");
            throw new IllegalArgumentException("Il comune non puo' essere nullo, registrazione >= Contributor");
        }
        validaCredenziali(contributorDTO.getTuristaDTO());

        return switch (ruolo) {
            case TURISTA -> registraTurista(contributorDTO.getTuristaDTO());
            case CONTRIBUTOR -> {
                Contributor contributor = new Contributor(contributorDTO);
                yield contributorService.save(contributor);
            }
            case CURATORE -> {
                Curatore curatore = new Curatore(contributorDTO);
                curatoreService.save(curatore);
                yield cambiaRuolo(contributorDTO.getTuristaDTO().getUsername(), Ruolo.CURATORE);
            }
            case ANIMATORE -> {
                Animatore animatore = new Animatore(contributorDTO);
                animatoreServiceI.save(animatore);
                yield cambiaRuolo(contributorDTO.getTuristaDTO().getUsername(), Ruolo.ANIMATORE);
            }
            case CONTRIBUTOR_AUTORIZZATO -> {
                ContributorAutorizzato contributor = new ContributorAutorizzato(contributorDTO);
                contributorAutorizzatoService.save(contributor);
                yield cambiaRuolo(contributorDTO.getTuristaDTO().getUsername(), Ruolo.CONTRIBUTOR_AUTORIZZATO);
            }
        };
    }
}
