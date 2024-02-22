package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.ContributorRepository;
import ids.unicam.Service.ContributorService;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import ids.unicam.models.Observer;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.notifiche.Notifica;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

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

    @Override
    public void deleteById(String id) {
        repository.deleteById(id);
    }

    @Override
    public Optional<Contributor> getById(String username) {
        return repository.findById(username);
    }

    @Override
    public Contributor update(RichiestaCreazioneContributorDTO contributorDTO, String username) {
        //TODO
        return null;
    }

    public Contributor save(Contributor contributor) {
        return repository.save(contributor);
    }


    public void deleteAll() {
        repository.deleteAll();
    }


    @Override
    public Itinerario aggiungiItinerario(Itinerario itinerario) {
        return itinerarioServiceImpl.creaItinerario(itinerario);
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
    public List<Contributor> getAll() {
        return repository.findAll();
    }

    @Override
    public List<Notifica> riceviNotifiche(Contributor contributor) {
        return notificaServiceImpl.getNotifiche(contributor);
    }

}
