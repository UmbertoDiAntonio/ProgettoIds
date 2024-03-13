package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.ContributorRepository;
import ids.unicam.Service.ContributorService;
import ids.unicam.models.attori.Contributor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

@Service
public class ContributorServiceImpl implements ContributorService {
    private final ContributorRepository repository;


    @Autowired
    public ContributorServiceImpl(ContributorRepository repository) {
        this.repository = repository;
    }

    @Override
    public List<Contributor> find(Predicate<Contributor> predicate) {
        List<Contributor> list = new ArrayList<>();
        for (Contributor contributor : getAll())
            if (predicate.test(contributor))
                list.add(contributor);
        return list;
    }

    @Override
    public void deleteByUsername(String id) {
        repository.deleteById(id);
    }

    @Override
    public Optional<Contributor> getByUsername(String username) {
        return repository.findById(username);
    }

    @Override
    public Contributor save(Contributor contributor) {
        return repository.save(contributor);
    }

    @Override
    public List<Contributor> getAll() {
        return repository.findAll();
    }


}
