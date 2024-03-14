package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.AnimatoreRepository;
import ids.unicam.Service.*;
import ids.unicam.exception.ContestException;
import ids.unicam.models.Invito;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import jakarta.transaction.Transactional;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.*;
import java.util.function.Predicate;

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
    public void deleteByUsername( @NotNull String username) {
        repository.deleteById(username);
    }

    @Override
    public  @NotNull Animatore save( @NotNull Animatore animatore) {
        animatore = repository.save(animatore);
        return animatore;
    }


    @Override
    public @NotNull Optional<Animatore> getByUsername(@NotNull String username) {
        return repository.findById(username);
    }


    @Override
    public void terminaContest( @NotNull String usernameAnimatore, int idContest) throws UnsupportedOperationException, IllegalArgumentException {
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
    public void setVincitoreContest( @NotNull String usernameAnimatore, int idContest, int idMateriale) throws ContestException, UnsupportedOperationException, IllegalArgumentException {
        Optional<Animatore> oAnimatore = getByUsername(usernameAnimatore);
        if (oAnimatore.isPresent()) {
            Animatore animatore = oAnimatore.get();
            Optional<Contest> oContest = contestService.findById(idContest);
            if (oContest.isPresent()) {
                Contest contest = oContest.get();
                Optional<MaterialeGenerico> oMateriale = materialeService.getById(idMateriale);
                if (oMateriale.isPresent()) {
                    MaterialeGenerico materiale = oMateriale.get();
                    if (animatore.equals(contest.getCreatore())) {
                        if (!contest.getMateriali().contains(materiale)) {
                            throw new ContestException("il materiale non risulta tra i materiali del contest");
                        }
                        if (!contest.isExpired()) {
                            throw new ContestException("Il Contest deve essere terminato per decretare un vincitore");
                        }
                        if (!contest.getPartecipanti().contains(materiale.getCreatore())) {
                            throw new ContestException("Vincitore non valido, l'utente ha lasciato il Contest");
                        } else {
                            contest.setMaterialeVincitore(materiale);
                            notificaService.creaNotificaVittoriaContest(contest.getCreatore(), contest, contest.getMaterialeVincitore());
                            contestService.save(contest);
                        }
                    } else {
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
    public void annullaInvito( @NotNull String usernameAnimatore, int idInvito) throws IllegalArgumentException, UnsupportedOperationException {
        Optional<Animatore> oAnimatore = getByUsername(usernameAnimatore);
        if (oAnimatore.isPresent()) {
            Animatore animatore = oAnimatore.get();
            Optional<Invito> oInvito = invitoService.findById(idInvito);
            if (oInvito.isPresent()) {
                Invito invito = oInvito.get();
                if (invito.getContest().getCreatore().equals(animatore)) {
                    invito.setValido(false);
                    invitoService.save(invito);
                } else {
                    throw new UnsupportedOperationException("Devi essere il creatore del contest");
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
    public  @NotNull List<Animatore> getAll() {
        return repository.findAll();
    }

    @Override
    public @NotNull List<Animatore> find(Predicate<Animatore> predicate) {
        List<Animatore> list = new ArrayList<>();
        for (Animatore animatore : getAll())
            if (predicate.test(animatore))
                list.add(animatore);
        return Collections.unmodifiableList(list);
    }

    @Override
    public  @NotNull Invito invitaContest( @NotNull String usernameAnimatore, int idContest,  @NotNull String invitato) throws ContestException, IllegalStateException, IllegalArgumentException {
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
                        throw new ContestException("L'invitato fa gia' parte del contest");
                    } else {
                        notificaService.creaNotificaInvitoContest(animatore, contest, turistaAutenticato);
                        return invitoService.save(new Invito(contest, turistaAutenticato));
                    }
                } else {
                    logger.error("username invitato non valido");
                    throw new IllegalArgumentException("username invitato non valido");
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
    public void approvaMateriale( @NotNull String usernameAnimatore, int idContest, int idMaterialeGenerico, boolean stato) throws UnsupportedOperationException, IllegalArgumentException {
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
                    if (materialeGenerico.getStato() != Stato.IN_ATTESA) {
                        logger.error("lo stato del materiale è già settato");
                        throw new UnsupportedOperationException("lo stato del materiale è già settato");
                    }
                    if (Stato.toStatus(stato) == Stato.IN_ATTESA) {
                        logger.error("non puoi settare stato in attesa");
                        throw new UnsupportedOperationException("non puoi settare stato in attesa");
                    }
                    if (contestService.getMaterialiContest(contest).contains(materialeGenerico)) {
                        materialeGenerico.setStato(Stato.toStatus(stato));
                        materialeService.save(materialeGenerico);
                    } else {
                        logger.error("Materiale non è nel Contest selezionato");
                        throw new UnsupportedOperationException("Materiale non è nel Contest selezionato");
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
            logger.error("username Animatore non valido");
            throw new IllegalArgumentException("username Animatore non valido");
        }
    }


    @Override
    public void setFineContest(int idContest,  @NotNull LocalDate dataFine, @NotNull  String usernameAnimatore) throws UnsupportedOperationException, IllegalArgumentException {
        Optional<Animatore> oAnimatore = getByUsername(usernameAnimatore);
        if (oAnimatore.isEmpty()) {
            throw new IllegalArgumentException("username animatore non valido");
        }
        Optional<Contest> oContest = contestService.findById(idContest);
        if (oContest.isEmpty()) {
            throw new IllegalArgumentException("id contest non valido");
        }
        Contest contest = oContest.get();
        if (contest.getCreatore().equals(oAnimatore.get())) {
            if (dataFine.isAfter(LocalDate.now())) {
                contest.setExpireDate(dataFine);
                contestService.save(contest);
            } else {
                logger.error("La scadenza deve essere una data futura");
                throw new IllegalArgumentException("La scadenza deve essere una data futura");
            }
        } else {
            logger.error("l'animatore non può impostare la data di fine contest per contest di cui non è creatore");
            throw new UnsupportedOperationException("l'animatore non può impostare la data di fine contest per contest di cui non è creatore");
        }
    }


    @Transactional
    @Override
    public void aggiungiTagContest(int idContest,  @NotNull String tag, @NotNull  String usernameAnimatore) throws ContestException, IllegalArgumentException, UnsupportedOperationException {
        Optional<Animatore> oAnimatore = getByUsername(usernameAnimatore);
        if (oAnimatore.isEmpty()) {
            throw new IllegalArgumentException("username non valido");
        }
        Animatore animatore = oAnimatore.get();

        Optional<Contest> oContest = contestService.findById(idContest);
        if (oContest.isPresent()) {
            Contest contest = oContest.get();
            if (tagService.haveTag(contest, tag)) {
                logger.warn("Tag già presente");
                throw new IllegalArgumentException("Tag già presente");
            }
            if (!Objects.equals(animatore.getUsername(), contest.getCreatore().getUsername())) {
                throw new UnsupportedOperationException(animatore.getUsername() + " non può modificare contest non suoi");
            }
            if (!contest.isExpired()) {
                tagService.aggiungiTag(contest, tag);
                contestService.save(contest);
            } else {
                logger.warn("Il Contest è terminato");
                throw new ContestException("Il Contest è terminato");
            }
        } else {
            logger.error("L'id del contest non e' valido");
            throw new IllegalArgumentException("L'id del contest non e' valido");
        }
    }


    @Transactional
    @Override
    public void rimuoviTagContest(int idContest,  @NotNull String tag, @NotNull  String usernameAnimatore) throws ContestException, IllegalArgumentException, UnsupportedOperationException {
        Optional<Animatore> oAnimatore = getByUsername(usernameAnimatore);
        if (oAnimatore.isEmpty()) {
            throw new IllegalArgumentException("username non valido");
        }
        Animatore animatore = oAnimatore.get();

        Optional<Contest> oContest = contestService.findById(idContest);
        if (oContest.isPresent()) {
            Contest contest = oContest.get();
            if (!tagService.haveTag(contest, tag)) {
                return;
            }
            if (!Objects.equals(animatore.getUsername(), contest.getCreatore().getUsername())) {
                throw new UnsupportedOperationException(animatore.getUsername() + " non può modificare contest non suoi");
            }
            if (!contest.isExpired()) {
                tagService.rimuoviTag(contest, tag);
                contestService.save(contest);
            } else {
                logger.warn("Il Contest è terminato");
                throw new ContestException("Il Contest è terminato");
            }
        } else {
            logger.error("L'id del contest non e' valido");
            throw new IllegalArgumentException("L'id del contest non e' valido");
        }
    }
}
