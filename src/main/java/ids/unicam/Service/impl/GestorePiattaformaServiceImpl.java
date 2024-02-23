package ids.unicam.Service.impl;

import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
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
    public TuristaAutenticato cambiaRuolo(String usernameContributor, @NotNull Ruolo ruolo) {
        Optional<Contributor> oContributor = contributorServiceImpl.getById(usernameContributor);
        if(oContributor.isEmpty()){
            logger.error("username contributor non valido");
            throw new IllegalArgumentException("username contribor non valido");
        }
        Contributor contributor = oContributor.get();
        rimuoviVecchioRuolo(contributor);
        TuristaAutenticatoDTO turistaAutenticatoDTO=new TuristaAutenticatoDTO(contributor.getNome(),contributor.getCognome(),contributor.getDataNascita(),contributor.getPassword(),contributor.getUsername());
        Contributor modificato = switch (ruolo) {
            case TURISTA -> {
                logger.error("Non puoi tornare un turista");
                yield null;
            }
            case CURATORE -> curatoreServiceImpl.save(new Curatore(new RichiestaCreazioneContributorDTO(contributor.getComune(),turistaAutenticatoDTO,Ruolo.CURATORE)));
            case ANIMATORE -> animatoreServiceImpl.save(new Animatore(new RichiestaCreazioneContributorDTO(contributor.getComune(),turistaAutenticatoDTO,Ruolo.ANIMATORE)));
            case CONTRIBUTOR_AUTORIZZATO ->
                    contributorAutorizzatoServiceImpl.save(new ContributorAutorizzato(new RichiestaCreazioneContributorDTO(contributor.getComune(),turistaAutenticatoDTO,Ruolo.CONTRIBUTOR_AUTORIZZATO)));
            case CONTRIBUTOR -> contributorServiceImpl.save(new Contributor(new RichiestaCreazioneContributorDTO(contributor.getComune(),turistaAutenticatoDTO,Ruolo.CONTRIBUTOR)));
        };

        poiServiceImpl.findAll().stream()
                .filter(puntoInteresse -> puntoInteresse.getCreatore() != null && puntoInteresse.getCreatore().getUsername().equals(contributor.getUsername()))
                .forEach(puntoInteresse -> {
                    puntoInteresse.setCreatore(modificato);
                    poiServiceImpl.save(puntoInteresse);
                });

        return modificato;
    }


    private void rimuoviVecchioRuolo(@NotNull Contributor contributor) {
        switch (contributor) {
            case Curatore curatore-> curatoreServiceImpl.deleteById(curatore.getUsername());
            case ContributorAutorizzato contributorAutorizzato ->
                    contributorAutorizzatoServiceImpl.deleteById(contributorAutorizzato.getUsername());
            case Animatore animatore-> animatoreServiceImpl.deleteById(animatore.getUsername());
            case Contributor contributor1 -> contributorServiceImpl.deleteById(contributor1.getUsername());
        }
    }

    public boolean validaCredenziali(TuristaAutenticatoDTO turistaDTO) {
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

    public TuristaAutenticato registraTurista(TuristaAutenticatoDTO turistaDTO) {
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
                yield cambiaRuolo(contributorDTO.getTuristaDTO().getUsername(), Ruolo.CURATORE);
            }
            case ANIMATORE -> {
                Animatore animatore = new Animatore(contributorDTO);
                animatoreServiceImpl.save(animatore);
                yield cambiaRuolo(contributorDTO.getTuristaDTO().getUsername(), Ruolo.ANIMATORE);
            }
            case CONTRIBUTOR_AUTORIZZATO -> {
                ContributorAutorizzato contributor = new ContributorAutorizzato(contributorDTO);
                contributorAutorizzatoServiceImpl.save(contributor);
                yield cambiaRuolo(contributorDTO.getTuristaDTO().getUsername(),Ruolo.CONTRIBUTOR_AUTORIZZATO);
            }

        };

    }


}
