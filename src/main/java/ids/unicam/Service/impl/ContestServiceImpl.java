package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.ContestRepository;
import ids.unicam.Service.ContestService;
import ids.unicam.exception.ContestException;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

@Service
public class ContestServiceImpl implements ContestService {
    private final ContestRepository repository;
    private final MaterialeServiceImpl materialeService;
    private final NotificaService notificaService;

    @Autowired
    public ContestServiceImpl(ContestRepository repository, MaterialeServiceImpl materialeService, NotificaService notificaService) {
        this.repository = repository;
        this.materialeService = materialeService;
        this.notificaService = notificaService;
    }

    public void deleteById(int id) {
        repository.deleteById(id);
    }

    @Override
    public Contest save(Contest contest) {
        return repository.save(contest);
    }

    public Optional<Contest> findById(int id) {
        return repository.findById(id);
    }

    public List<Contest> findAll() {
        return repository.findAll();
    }


    @Override
    public Contest creaContest(String nomeContest, String obiettivo, Animatore creatore, boolean open) {
        return save(new Contest(nomeContest, obiettivo, creatore, open));
    }


    @Override
    public List<Taggable> find(Predicate<Contest> predicate) {
        List<Taggable> list =new ArrayList<>();
        for(Contest contest : findAll())
            if(predicate.test(contest))
                list.add(contest);
        return Collections.unmodifiableList(list);
    }





    @Transactional
    @Override
    public void aggiungiMateriale(String usernameTurista, int idContest, MaterialeGenerico materialeGenerico) throws ContestException, IllegalArgumentException {
        TuristaAutenticato turistaAutenticato = null;
        Optional<Contest> oContest = findById(idContest);
        if (oContest.isEmpty()) {
            throw new IllegalArgumentException("id contest non valido");
        }

        Contest contest = oContest.get();
        if (contest.isExpired()) {
            throw new ContestException("il Contest e' Terminato");
        }

        if (contest.isOpen()) {
            aggiungiPartecipante(contest, materialeGenerico.getCreatore());
        }
        if (!contest.isOpen())
            for (TuristaAutenticato turistaAutenticato1 : contest.getPartecipanti()) {
                if (turistaAutenticato1.getUsername().equals(usernameTurista)) {
                    turistaAutenticato = turistaAutenticato1;
                }
            }
        if (turistaAutenticato == null && !contest.isOpen()) {
            throw new ContestException("Devi essere iscritto al contest per caricare materiale su di esso");
        }

        materialeService.aggiungiMateriale(contest, materialeGenerico);
        save(contest);
    }


    @Override
    public List<TuristaAutenticato> getPartecipanti(Contest contest) {
        return Collections.unmodifiableList(repository.findPartecipantiByContest(contest.getId()));
    }

    /**
     * Aggiungi un partecipante a un contest
     * @param contest il contest in cui si vuole aggiungere il partecipante
     * @param turistaAutenticato il partecipante che si vuole aggiungere
     * @throws ContestException se il contest è terminato
     */
    @Transactional
    void aggiungiPartecipante(Contest contest, TuristaAutenticato turistaAutenticato) throws ContestException {
        if (contest.isExpired()) {
            throw new ContestException("il Contest e' Terminato");
        }
        contest.aggiungiPartecipante(turistaAutenticato);
        notificaService.creaNotificaIngressoContest(contest, turistaAutenticato);
        save(contest);
    }

    @Transactional
    @Override
    public void setVincitoreContest(Contest contest, MaterialeGenerico materiale) throws ContestException {
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
            save(contest);
        }
    }


    /**
     * Termina un Contest, successivamente l'animatore dovrà decretare il vincitore
     *
     * @param contest il contest da terminare
     */
    @Transactional
    @Override
    public void terminaContest(Contest contest) {
        contest.setExpireDate(LocalDate.now());

        for (TuristaAutenticato turistaAutenticato : contest.getPartecipanti())
            notificaService.creaNotificaTermineContest(contest, turistaAutenticato);

        save(contest);
    }

    @Override
    public List<MaterialeGenerico> getMaterialiContest(Contest contest) {
        return repository.getMateriali(contest.getId());
    }

    @Override
    public Optional<Contest> getContestContainingMaterial(MaterialeGenerico materialeGenerico) {
        return repository.findContestByMaterialiContaining(materialeGenerico);
    }

    @Override
    @Transactional
    public void checkIfIsExpired(Contest contest) {
        if (contest.isExpired()) {
            terminaContest(contest);
        }
    }

    @Override
    public List<Contest> getContestByComune(String nomeComune) {
        return repository.findContestByComune(nomeComune);
    }
}

