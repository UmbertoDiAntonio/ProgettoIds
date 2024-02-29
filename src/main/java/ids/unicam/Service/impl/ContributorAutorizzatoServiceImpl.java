package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.ContributorAutorizzatoRepository;
import ids.unicam.Service.ContributorAutorizzatoService;
import ids.unicam.models.attori.ContributorAutorizzato;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class ContributorAutorizzatoServiceImpl implements ContributorAutorizzatoService {
    private final ContributorAutorizzatoRepository repository;

    @Autowired
    public ContributorAutorizzatoServiceImpl(ContributorAutorizzatoRepository repository) {
        this.repository = repository;
    }

    @Override
    public void deleteByUsername(String id) {
        repository.deleteById(id);
    }

    @Override
    public Optional<ContributorAutorizzato> getByUsername(String username) {
        return repository.findById(username);
    }

    @Override
    public ContributorAutorizzato save(ContributorAutorizzato contributorAutorizzato) {
        return repository.save(contributorAutorizzato);
    }

    @Override
    public List<ContributorAutorizzato> getAll() {
        return repository.findAll();
    }

    @Override
    public List<ContributorAutorizzato> findByNomeComune(String nomeComune) {
        return repository.findByComuneNome(nomeComune);
    }


}
