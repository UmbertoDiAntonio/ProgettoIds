package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.AnimatoreRepository;
import ids.unicam.Service.*;
import ids.unicam.exception.ContestException;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.Invito;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.puntiInteresse.Tag;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import static ids.unicam.Main.logger;

@Service
public class AnimatoreServiceImpl implements AnimatoreService {
    private final AnimatoreRepository repository;
    private final ContestService contestService;
    private final InvitoService invitoService;
    private final TuristaAutenticatoService turistaAutenticatoService;
    private final MaterialeService materialeService;
    private final NotificaService notificaService;
    private final TagService tagService;


    @Autowired
    public AnimatoreServiceImpl(AnimatoreRepository repository, ContestService contestService, InvitoService invitoService, TuristaAutenticatoService turistaAutenticatoService, MaterialeService materialeService, NotificaService notificaService, TagService tagService) {
        this.repository = repository;
        this.contestService = contestService;
        this.invitoService = invitoService;
        this.turistaAutenticatoService = turistaAutenticatoService;
        this.materialeService = materialeService;
        this.notificaService = notificaService;
        this.tagService = tagService;
    }

    @Override
    public void deleteByUsername(String username) {
        repository.deleteById(username);
    }

    @Override
    public Optional<Animatore> getByUsername(String username) {
        return repository.findById(username);
    }

    @Override
    public void terminaContest(String usernameAnimatore, int idContest) throws UnsupportedOperationException, IllegalArgumentException, ContestException {
        Optional<Animatore> oAnimatore = getByUsername(usernameAnimatore);
        if (oAnimatore.isPresent()) {
            Animatore animatore = oAnimatore.get();
            Optional<Contest> oContest = contestService.findById(idContest);
            if (oContest.isPresent()) {
                Contest contest = oContest.get();
                if (animatore.equals(contest.getCreatore()))
                    contestService.terminaContest(contest);
                else {
                    logger.error("L'animatore deve essere il creatore del contest");
                    throw new UnsupportedOperationException("L'animatore deve essere il creatore del contest");
                }
            } else {
                logger.error("id Contest non valido");
                throw new IllegalArgumentException("id Contest non valido");
            }
        } else {
            logger.error("Username Animatore non valido");
            throw new IllegalArgumentException("Username Animatore non valido");
        }
    }

    @Override
    public void setVincitoreContest(String usernameAnimatore, int idContest, int idMateriale) throws ContestException {
        Optional<Animatore> oAnimatore = getByUsername(usernameAnimatore);
        if (oAnimatore.isPresent()) {
            Animatore animatore = oAnimatore.get();
            Optional<Contest> oContest = contestService.findById(idContest);
            if (oContest.isPresent()) {
                Contest contest = oContest.get();
                Optional<MaterialeGenerico> oMateriale = materialeService.getById(idMateriale);
                if (oMateriale.isPresent()) {
                    MaterialeGenerico materiale = oMateriale.get();
                    if (animatore.equals(contest.getCreatore()))
                        contestService.setVincitoreContest(contest, materiale);
                    else {
                        logger.error("L'animatore deve essere il creatore del contest");
                        throw new UnsupportedOperationException("L'animatore deve essere il creatore del contest");
                    }
                } else {
                    logger.error("id Materiale non valido");
                    throw new IllegalArgumentException("id Materiale non valido");
                }
            } else {
                logger.error("id Contest non valido");
                throw new IllegalArgumentException("id Contest non valido");
            }
        } else {
            logger.error("Username Animatore non valido");
            throw new IllegalArgumentException("Username Animatore non valido");
        }
    }

    @Override
    public void annullaInvito(String usernameAnimatore, int idInvito) throws IllegalArgumentException, ContestException {
        Optional<Animatore> oAnimatore = getByUsername(usernameAnimatore);
        if (oAnimatore.isPresent()) {
            Animatore animatore = oAnimatore.get();
            Optional<Invito> oInvito = invitoService.findById(idInvito);
            if (oInvito.isPresent()) {
                Invito invito = oInvito.get();
                if (invito.getContest().getCreatore().equals(animatore)) {
                    invito.setValido(false);
                } else {
                    throw new ContestException("Devi essere il creatore del contest");
                }
            } else {
                logger.error("id Invito non valido");
                throw new IllegalArgumentException("id Invito non valido");
            }
        } else {
            logger.error("Username Animatore non valido");
            throw new IllegalArgumentException("Username Animatore non valido");
        }
    }

    @Override
    public List<Animatore> getAll() {
        return repository.findAll();
    }

    @Override
    public Animatore save(Animatore animatore) {
        animatore = repository.save(animatore);
        return animatore;
    }

    @Override
    public List<Animatore> findByNomeComune(String nomeComune) {
        return repository.findByComuneNome(nomeComune);
    }


    @Override
    public Invito invitaContest(String usernameAnimatore, int idContest, String invitato) throws ContestException, IllegalStateException, IllegalArgumentException {
        Optional<Animatore> oAnimatore = getByUsername(usernameAnimatore);
        if (oAnimatore.isPresent()) {
            Animatore animatore = oAnimatore.get();
            Optional<Contest> oContest = contestService.findById(idContest);
            if (oContest.isPresent()) {
                Contest contest = oContest.get();
                if (!contest.getCreatore().equals(animatore)) {
                    logger.error("L'animatore non e' il creatore del contest.");
                    throw new IllegalStateException("L'animatore non e' il creatore del contest.");
                }
                Optional<TuristaAutenticato> oTurista = turistaAutenticatoService.getByUsername(invitato);
                if (oTurista.isPresent()) {
                    TuristaAutenticato turistaAutenticato = oTurista.get();
                    if (contestService.getPartecipanti(contest).contains(turistaAutenticato)) {
                        logger.error("Il turista autenticato fa gia' parte del contest");
                        throw new ContestException("Il turista autenticato fa gia' parte del contest");
                    } else {
                        notificaService.creaNotificaInvitoContest(animatore, contest, turistaAutenticato);
                        return invitoService.save(new Invito(contest, turistaAutenticato));
                    }
                } else {
                    logger.error("username del turista invitato non valido");
                    throw new IllegalArgumentException("username del turista invitato non valido");
                }
            } else {
                logger.error("id del contest non valido");
                throw new IllegalArgumentException("id del contest non valido");
            }
        } else {
            logger.error("username dell'animatore non valido");
            throw new IllegalArgumentException("username dell'animatore non valido");
        }
    }

    @Override
    public boolean approvaMateriale(String usernameAnimatore, int idContest, int idMaterialeGenerico, boolean stato) throws UnsupportedOperationException, IllegalArgumentException {
        Optional<Animatore> oAnimatore = getByUsername(usernameAnimatore);
        if (oAnimatore.isPresent()) {
            Animatore animatore = oAnimatore.get();
            Optional<Contest> oContest = contestService.findById(idContest);
            if (oContest.isPresent()) {
                Contest contest = oContest.get();
                if (!contest.getCreatore().equals(animatore)) {
                    logger.warn(animatore + "  non è autorizzato ad approvare nel contest " + contest);
                    throw new UnsupportedOperationException(animatore + "  non è autorizzato ad approvare nel contest " + contest);
                }
                Optional<MaterialeGenerico> oMateriale = materialeService.getById(idMaterialeGenerico);
                if (oMateriale.isPresent()) {
                    MaterialeGenerico materialeGenerico = oMateriale.get();
                    if (materialeGenerico.getStato() != Stato.IN_ATTESA)
                        throw new UnsupportedOperationException("materiale già settato");
                    if (Stato.toStatus(stato) == Stato.IN_ATTESA)
                        throw new UnsupportedOperationException("non puoi settare stato in attesa");
                    materialeService.approvaMateriale(materialeGenerico, Stato.toStatus(stato));
                    return true;
                } else {
                    logger.error("id Materiale non valido");
                    throw new IllegalArgumentException("id Materiale non valido");
                }
            } else {
                logger.error("id Contest non valido");
                throw new IllegalArgumentException("id Contest non valido");
            }
        } else {
            logger.error("username Animatore non valido");
            throw new IllegalArgumentException("username Animatore non valido");
        }
    }

    @Override
    public void setFineContest(int idContest, LocalDate dataFine, String usernameAnimatore) throws FuoriComuneException {
        Optional<Animatore> oAnimatore =getByUsername(usernameAnimatore);
        if (oAnimatore.isEmpty()) {
            throw new FuoriComuneException("username animatore non valido");
        }

        Optional<Contest> oContest = contestService.findById(idContest);
        if (oContest.isEmpty()) {
            throw new FuoriComuneException("id contest non valido");
        }

        Contest contest = oContest.get();

        contest.setExpireDate(dataFine);
        contestService.save(contest);
    }

    @Transactional
    @Override
    public void aggiungiTagContest(int idContest, Tag tag, String usernameAnimatore) throws ContestException {
        Optional<Animatore> oAnimatore = getByUsername(usernameAnimatore);
        if (oAnimatore.isEmpty()) {
            throw new IllegalArgumentException("username non valido");
        }
        Animatore animatore = oAnimatore.get();
        //TODO

        Optional<Contest> oContest = contestService.findById(idContest);
        if (oContest.isPresent()) {
            Contest contest = oContest.get();
            if (tagService.haveTag(contest, tag)) {
                logger.warn("Tag già aggiunto");
                return;
            }
            if(!Objects.equals(animatore.getUsername(), contest.getCreatore().getUsername())){
                throw new ContestException(animatore.getUsername()+" non può modificare contest non suoi");
            }
            if (!contest.isExpired())
                tagService.aggiungiTag(contest, tag);
            contestService.save(contest);
        } else {
            logger.error("L'id del contest non e' valido");
            throw new IllegalArgumentException("L'id del contest non e' valido");
        }
    }

    @Transactional
    @Override
    public void rimuoviTagContest(int idContest, Tag tag, String usernameAnimatore) throws ContestException {
        Optional<Animatore> oAnimatore = getByUsername(usernameAnimatore);
        if (oAnimatore.isEmpty()) {
            throw new IllegalArgumentException("username non valido");
        }
        Animatore animatore = oAnimatore.get();
        //TODO

        Optional<Contest> oContest = contestService.findById(idContest);
        if (oContest.isPresent()) {
            Contest contest = oContest.get();
            if (!tagService.haveTag(contest, tag)) {
                return;
            }
            if(!Objects.equals(animatore.getUsername(), contest.getCreatore().getUsername())){
                throw new ContestException(animatore.getUsername()+" non può modificare contest non suoi");
            }
            if (!contest.isExpired())
                tagService.rimuoviTag(contest, tag);
            contestService.save(contest);
        } else {
            logger.error("L'id del contest non e' valido");
            throw new IllegalArgumentException("L'id del contest non e' valido");
        }
    }
}
