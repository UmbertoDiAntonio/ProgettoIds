package ids.unicam.Service.impl;

import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.models.Comune;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import ids.unicam.models.DTO.RichiestaCreazioneTuristaDTO;
import ids.unicam.models.attori.*;
import jakarta.transaction.Transactional;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.GregorianCalendar;

import static ids.unicam.Main.logger;
import static ids.unicam.models.attori.Ruolo.CONTRIBUTOR_AUTORIZZATO;

@Service
public class GestorePiattaformaServiceImpl implements GestorePiattaformaService {
    private final ContributorServiceImpl contributorServiceImpl;
    private final TuristaAutenticatoServiceImpl turistaAutenticatoServiceImpl;
    private final CuratoreServiceImpl curatoreServiceImpl;
    private final AnimatoreServiceImpl animatoreServiceImpl;
    private final ContributorAutorizzatoServiceImpl contributorAutorizzatoServiceImpl;
    private final PoiServiceImpl poiServiceImpl;

    @Autowired
    public GestorePiattaformaServiceImpl(ContributorServiceImpl contributorServiceImpl, TuristaAutenticatoServiceImpl turistaAutenticatoServiceImpl,
                                         CuratoreServiceImpl curatoreServiceImpl, AnimatoreServiceImpl animatoreServiceImpl, ContributorAutorizzatoServiceImpl contributorAutorizzatoServiceImpl, PoiServiceImpl poiServiceImpl) {
        this.contributorServiceImpl = contributorServiceImpl;
        this.turistaAutenticatoServiceImpl = turistaAutenticatoServiceImpl;
        this.animatoreServiceImpl = animatoreServiceImpl;
        this.curatoreServiceImpl = curatoreServiceImpl;
        this.contributorAutorizzatoServiceImpl = contributorAutorizzatoServiceImpl;
        this.poiServiceImpl = poiServiceImpl;
    }


    @Transactional
    @Override
    public TuristaAutenticato cambiaRuolo(RichiestaCreazioneContributorDTO contributorDTO, @NotNull Ruolo ruolo) {
        rimuoviVecchioRuolo(contributorDTO);
        Contributor modificato = switch (ruolo) {
            case TURISTA -> {
                logger.error("Non puoi tornare un turista");
                yield null;
            }
            case CURATORE -> curatoreServiceImpl.save(new Curatore(contributorDTO));
            case ANIMATORE -> animatoreServiceImpl.save(new Animatore(contributorDTO));
            case CONTRIBUTOR_AUTORIZZATO ->
                    contributorAutorizzatoServiceImpl.save(new ContributorAutorizzato(contributorDTO));
            case CONTRIBUTOR -> contributorServiceImpl.save(new Contributor(contributorDTO));
        };

        poiServiceImpl.findAll().stream()
                .filter(puntoInteresse -> puntoInteresse.getCreatore() != null && puntoInteresse.getCreatore().getUsername().equals(contributorDTO.getTuristaDTO().getUsername()))
                .forEach(puntoInteresse -> {
                    puntoInteresse.setCreatore(modificato);
                    poiServiceImpl.save(puntoInteresse);
                });

        return modificato;
    }


    private void rimuoviVecchioRuolo(@NotNull RichiestaCreazioneContributorDTO contributorDTO) {
        switch (contributorDTO.getRuolo()) {
            case CURATORE -> curatoreServiceImpl.deleteById(contributorDTO.getTuristaDTO().getUsername());
            case CONTRIBUTOR_AUTORIZZATO ->
                    contributorAutorizzatoServiceImpl.deleteById(contributorDTO.getTuristaDTO().getUsername());
            case ANIMATORE -> animatoreServiceImpl.deleteById(contributorDTO.getTuristaDTO().getUsername());
            case CONTRIBUTOR -> contributorServiceImpl.deleteById(contributorDTO.getTuristaDTO().getUsername());
        }
    }

    public boolean validaCredenziali(RichiestaCreazioneTuristaDTO turistaDTO) {
        if (!turistaDTO.getPassword().matches("^(?=.*[A-Z])(?=.*[@#$%^&+=])(?=.*[0-9])(?=.*[a-zA-Z]).{6,}$")) {
            logger.error("Password non valida");
            throw new IllegalArgumentException("Password non valida");
            //TODO messaggio da parte dell'interfaccia
        }
        if (!turistaDTO.getUsername().matches("^.{5,}$")) {
            logger.error("Username non valido");
            throw new IllegalArgumentException("Username non valido");
            //TODO messaggio da parte dell'interfaccia
        }
        if (!turistaAutenticatoServiceImpl.isUsernameUnique(turistaDTO.getUsername())) {
            logger.error("Username già esistente");
            throw new IllegalArgumentException("Username già esistente");
            //TODO messaggio da parte dell'interfaccia
        }
        return true;
    }

    public TuristaAutenticato registraTurista(RichiestaCreazioneTuristaDTO turistaDTO) {
        if (!validaCredenziali(turistaDTO)) {
            return null;
        }
        TuristaAutenticato nuovoTurista = new TuristaAutenticato(turistaDTO);
        return turistaAutenticatoServiceImpl.save(nuovoTurista);
    }

    @Transactional
    public TuristaAutenticato registraContributor(RichiestaCreazioneContributorDTO contributorDTO) {
        if (contributorDTO.getRuolo() != Ruolo.TURISTA && contributorDTO.getComune() == null) {
            logger.error("Il comune non puo' essere nullo, registrazione >= Contributor");
            throw new RuntimeException("Il comune non puo' essere nullo, registrazione >= Contributor");
        }
        if (!validaCredenziali(contributorDTO.getTuristaDTO())) {
            return null;
        }
        return switch (contributorDTO.getRuolo()) {
            case TURISTA -> registraTurista(contributorDTO.getTuristaDTO());
            case CONTRIBUTOR -> {
                Contributor contributor = new Contributor(contributorDTO);
                yield contributorServiceImpl.save(contributor);
            }
            case CURATORE-> {
                Curatore curatore = new Curatore(contributorDTO);
                curatoreServiceImpl.save(curatore);
                yield cambiaRuolo(contributorDTO,Ruolo.CURATORE);
            }
            case ANIMATORE -> {
                Animatore animatore = new Animatore(contributorDTO);
                animatoreServiceImpl.save(animatore);
                yield cambiaRuolo(contributorDTO,Ruolo.ANIMATORE);
            }
            case CONTRIBUTOR_AUTORIZZATO -> {
                ContributorAutorizzato contributor = new ContributorAutorizzato(contributorDTO);
                contributorAutorizzatoServiceImpl.save(contributor);
                yield cambiaRuolo(contributorDTO,Ruolo.CONTRIBUTOR_AUTORIZZATO);
            }

        };

    }


}
