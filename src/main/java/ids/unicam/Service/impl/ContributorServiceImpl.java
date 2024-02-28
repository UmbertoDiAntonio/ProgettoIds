package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.ContributorRepository;
import ids.unicam.Service.ContributorService;
import ids.unicam.models.Observer;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.contenuti.notifiche.Notifica;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class ContributorServiceImpl implements ContributorService{
    private final ContributorRepository repository;
    private final NotificaServiceImpl notificaServiceImpl;

    @Autowired
    public ContributorServiceImpl(ContributorRepository repository,
                                  NotificaServiceImpl notificaServiceImpl) {
        this.repository = repository;
        this.notificaServiceImpl = notificaServiceImpl;
    }

    public List<Contributor> findByNomeComune(String nomeComune) {
        return repository.findByComuneNome(nomeComune);
    }

    @Override
    public void deleteById(String id) {
        repository.deleteById(id);
    }

    @Override
    public Optional<Contributor> getById(String username) {
        return repository.findById(username);
    }

    public Contributor save(Contributor contributor) {
        return repository.save(contributor);
    }


    public void deleteAll() {
        repository.deleteAll();
    }
    @Override
    public List<Contributor> getAll() {
        return repository.findAll();
    }


    public List<Notifica> notifica(Contributor contributor) {//TODO notifiche Controller?
        return notificaServiceImpl.getNotifiche(contributor);
    }

}
