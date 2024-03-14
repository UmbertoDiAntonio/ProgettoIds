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
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
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
    private final NotificaService notificaService;

    @Autowired
    public ContestServiceImpl(ContestRepository repository, NotificaService notificaService) {
        this.repository = repository;
        this.notificaService = notificaService;
    }

    public void deleteById(int id) {
        repository.deleteById(id);
    }

    @Override
    public @NotNull Contest save(@NotNull Contest contest) {
        return repository.save(contest);
    }


    public @NotNull Optional<Contest> findById(@NotNull int id) {
        return repository.findById(id);
    }


    public @NotNull List<Contest> findAll() {
        return Collections.unmodifiableList(repository.findAll());
    }


    @Override
    public @NotNull Contest creaContest(@NotNull String nomeContest, @NotNull String obiettivo, @NotNull Animatore creatore, boolean open) {
        return save(new Contest(nomeContest, obiettivo, creatore, open));
    }


    @Override
    public @NotNull List<Taggable> find(@Nullable Predicate<Contest> predicate) {

        List<Taggable> list = new ArrayList<>();
        for (Contest contest : findAll())
            if (predicate != null) {
                if (predicate.test(contest))
                    list.add(contest);
            } else {
                list.add(contest);
            }

        return Collections.unmodifiableList(list);
    }


    @Override
    public @NotNull List<TuristaAutenticato> getPartecipanti( @NotNull Contest contest) {
        return Collections.unmodifiableList(repository.findPartecipantiByContest(contest.getId()));
    }

    /**
     * Aggiungi un partecipante a un contest
     *
     * @param contest            il contest in cui si vuole aggiungere il partecipante
     * @param turistaAutenticato il partecipante che si vuole aggiungere
     * @throws ContestException se il contest è terminato
     */
    @Transactional
    void aggiungiPartecipante(@NotNull Contest contest, @NotNull TuristaAutenticato turistaAutenticato) throws ContestException {
        if (contest.isExpired()) {
            throw new ContestException("il Contest e' Terminato");
        }
        contest.aggiungiPartecipante(turistaAutenticato);
        notificaService.creaNotificaIngressoContest(contest, turistaAutenticato);
        save(contest);
    }


    @Transactional
    @Override
    public void terminaContest(@NotNull Contest contest) {
        contest.setExpireDate(LocalDate.now());
        for (TuristaAutenticato turistaAutenticato : contest.getPartecipanti())
            notificaService.creaNotificaTermineContest(contest, turistaAutenticato);
        save(contest);
    }

    @Override
    public @NotNull List<MaterialeGenerico> getMaterialiContest( @NotNull Contest contest) {
        return repository.getMateriali(contest.getId());
    }

    @Override
    public @NotNull Optional<Contest> getContestContainingMaterial(@NotNull MaterialeGenerico materialeGenerico) {
        return repository.findContestByMaterialiContaining(materialeGenerico);
    }


    @Override
    @Transactional
    public void checkIfIsExpired(@NotNull Contest contest) {
        if (contest.isExpired()) {
            terminaContest(contest);
        }
    }

    @Override
    public List<Contest> getContest( @Nullable Predicate<Contest> predicate) {
        if(predicate==null)
            return findAll();
        List<Contest> list = new ArrayList<>();
        for (Contest contest : findAll())
            if (predicate.test(contest))
                list.add(contest);
        return list;
    }
}

