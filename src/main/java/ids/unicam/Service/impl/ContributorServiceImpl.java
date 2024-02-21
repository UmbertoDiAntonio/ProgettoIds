package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.ContributorRepository;
import ids.unicam.Service.ContributorService;
import ids.unicam.models.Comune;
import ids.unicam.models.Notifica;
import ids.unicam.models.Observer;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

import static ids.unicam.Main.logger;

@Service
public class ContributorServiceImpl implements ContributorService, Observer {
    private final ContributorRepository repository;
    private final PoiServiceImpl poiServiceImpl;
    private final ItinerarioServiceImpl itinerarioServiceImpl;
    private final NotificaServiceImpl notificaServiceImpl;

    @Autowired
    public ContributorServiceImpl(ContributorRepository repository, PoiServiceImpl poiServiceImpl,
                                  ItinerarioServiceImpl itinerarioServiceImpl, NotificaServiceImpl notificaServiceImpl) {
        this.repository = repository;
        this.poiServiceImpl = poiServiceImpl;
        this.itinerarioServiceImpl = itinerarioServiceImpl;
        this.notificaServiceImpl = notificaServiceImpl;
    }

    public List<Contributor> findByNomeComune(String nomeComune) {
        return repository.findByComuneNome(nomeComune);
    }

    public void deleteById(int id) {
        repository.deleteById(id);
    }


    public Contributor save(Contributor contributor) {
        return repository.save(contributor);
    }


    public Optional<Contributor> findById(int id) {
        return repository.findById(id);
    }


    public List<Contributor> findAll() {
        return repository.findAll();
    }

    public Contributor getLast() {
        return repository.getLast();
    }

    public Contributor getFirst() {
        return repository.getFirst();
    }


    public void deleteAll() {
        repository.deleteAll();
    }


    @Override
    public Itinerario aggiungiItinerario(Comune comune, String nome, PuntoInteresse... puntoInteresse) {
        return itinerarioServiceImpl.creaItinerario(comune, nome, puntoInteresse);
    }

    @Transactional
    @Override
    public boolean aggiungiTappaItinerario(Itinerario itinerario, PuntoInteresse puntoInteresse) {
        return itinerarioServiceImpl.aggiungiTappa(itinerario, puntoInteresse);
    }

    @Override
    public void modificaScadenza(PuntoInteresse puntoInteresse, LocalDate expireDate) {
        puntoInteresse.setExpireDate(expireDate);
        poiServiceImpl.save(puntoInteresse);
    }

    @Override
    public List<Notifica> riceviNotifiche(Contributor contributor) {
        return notificaServiceImpl.getNotifiche(contributor);
    }

}
