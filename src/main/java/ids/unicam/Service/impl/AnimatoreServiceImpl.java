package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.AnimatoreRepository;
import ids.unicam.Service.AnimatoreService;
import ids.unicam.Service.MaterialeService;
import ids.unicam.Service.TuristaAutenticatoService;
import ids.unicam.exception.ContestException;
import ids.unicam.models.Invito;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

import static ids.unicam.Main.logger;

@Service
public class AnimatoreServiceImpl implements AnimatoreService {
    private final AnimatoreRepository repository;
    private final ContestServiceImpl contestService;
    private final InvitoServiceImpl invitoServiceImpl;
    private final TuristaAutenticatoService turistaAutenticatoService;
    private final MaterialeService materialeService;


    @Autowired
    public AnimatoreServiceImpl(AnimatoreRepository repository, ContestServiceImpl contestService, InvitoServiceImpl invitoServiceImpl, MaterialeServiceImpl materialeServiceImpl, TuristaAutenticatoService turistaAutenticatoService, MaterialeService materialeService) {
        this.repository = repository;
        this.contestService = contestService;
        this.invitoServiceImpl = invitoServiceImpl;
        this.turistaAutenticatoService = turistaAutenticatoService;
        this.materialeService = materialeService;
    }

    @Override
    public void deleteById(String id) {
        repository.deleteById(id);
    }

    @Override
    public Optional<Animatore> getById(String username) {
        return repository.findById(username);
    }

    @Override
    public void terminaContest(String idAnimatore, Integer idContest, Integer idMateriale) throws ContestException, UnsupportedOperationException, IllegalArgumentException {
        Optional<Animatore> oAnimatore = getById(idAnimatore);
        if (oAnimatore.isPresent()) {
            Animatore animatore = oAnimatore.get();
            Optional<Contest> oContest = contestService.findById(idContest);
            if (oContest.isPresent()) {
                Contest contest = oContest.get();
                if (animatore.equals(contest.getCreatore()))
                    contestService.terminaContest(contest, idMateriale);
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
    public void annullaInvito(String usernameAnimatore, int idInvito) throws IllegalArgumentException, ContestException {
        Optional<Animatore> oAnimatore = getById(usernameAnimatore);
        if (oAnimatore.isPresent()) {
            Animatore animatore = oAnimatore.get();
            Optional<Invito> oInvito = invitoServiceImpl.findById(idInvito);
            if (oInvito.isPresent()) {
                Invito invito = oInvito.get();
                if(invito.getContest().getCreatore().equals(animatore)){
                    invito.setValido(false);
                }else {
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

    public Animatore save(Animatore animatore) {
        animatore = repository.save(animatore);
        return animatore;
    }

    public void deleteAll() {
        repository.deleteAll();
    }


    public List<Animatore> findByNomeComune(String nomeComune) {
        return repository.findByComuneNome(nomeComune);
    }


    @Override
    public Invito invitaContest(String usernameAnimatore, Integer idContest, String invitato) throws ContestException, IllegalStateException, IllegalArgumentException {
        Optional<Animatore> oAnimatore = getById(usernameAnimatore);
        if (oAnimatore.isPresent()) {
            Animatore animatore = oAnimatore.get();
            Optional<Contest> oContest = contestService.findById(idContest);
            if (oContest.isPresent()) {
                Contest contest = oContest.get();
                if (!contest.getCreatore().equals(animatore)) {
                    logger.error("L'animatore non e' il creatore del contest.");
                    throw new IllegalStateException("L'animatore non e' il creatore del contest.");
                }
                Optional<TuristaAutenticato> oTurista = turistaAutenticatoService.getById(invitato);
                if (oTurista.isPresent()) {
                    TuristaAutenticato turistaAutenticato = oTurista.get();
                    if (contestService.getPartecipanti(contest).contains(turistaAutenticato)) {
                        logger.error("Il turista autenticato fa gia' parte del contest");
                        throw new ContestException("Il turista autenticato fa gia' parte del contest");
                    } else
                        return invitoServiceImpl.save(new Invito(contest, turistaAutenticato));
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
    public boolean approvaMateriale(String usernameAnimatore, Integer idContest, Integer idMaterialeGenerico, boolean stato) throws UnsupportedOperationException, IllegalArgumentException {
        Optional<Animatore> oAnimatore = getById(usernameAnimatore);
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


}
