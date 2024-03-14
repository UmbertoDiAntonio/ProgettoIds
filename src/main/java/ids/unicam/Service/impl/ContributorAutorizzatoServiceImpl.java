package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.ContributorAutorizzatoRepository;
import ids.unicam.Service.ContributorAutorizzatoService;
import ids.unicam.models.attori.ContributorAutorizzato;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

@Service
public class ContributorAutorizzatoServiceImpl implements ContributorAutorizzatoService {
    private final ContributorAutorizzatoRepository repository;

    @Autowired
    public ContributorAutorizzatoServiceImpl(ContributorAutorizzatoRepository repository) {
        this.repository = repository;
    }

    @Override
    public void deleteByUsername(@NotNull String id) {
        repository.deleteById(id);
    }

    @Override
    public @NotNull Optional<ContributorAutorizzato> getByUsername(@NotNull String username) {
        return repository.findById(username);
    }

    @Override
    public @NotNull ContributorAutorizzato save(@NotNull ContributorAutorizzato contributorAutorizzato) {
        return repository.save(contributorAutorizzato);
    }

    @Override
    public @NotNull List<ContributorAutorizzato> getAll() {
        return repository.findAll();
    }

    @Override
    public @NotNull List<ContributorAutorizzato> find( @Nullable Predicate<ContributorAutorizzato> predicate) {
        if(predicate==null)
            return getAll();
        List<ContributorAutorizzato> list = new ArrayList<>();
        for (ContributorAutorizzato contributorAutorizzato : getAll())
            if (predicate.test(contributorAutorizzato))
                list.add(contributorAutorizzato);
        return list;
    }


}
