package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.ContributorAutorizzatoRepository;
import ids.unicam.Service.ContributorAutorizzatoService;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Service
public class ContributorAutorizzatoServiceImpl implements ContributorAutorizzatoService {
    private final ContributorAutorizzatoRepository repository;

    private final ItinerarioServiceImpl itinerarioServiceImpl;

    @Autowired
    public ContributorAutorizzatoServiceImpl(ContributorAutorizzatoRepository repository, ItinerarioServiceImpl itinerarioServiceImpl) {
        this.repository = repository;
        this.itinerarioServiceImpl = itinerarioServiceImpl;
    }

    @Override
    public void deleteById(String id) {
        repository.deleteById(id);
    }

    @Override
    public Optional<ContributorAutorizzato> getById(String username) {
        return repository.findById(username);
    }

    @Override
    public ContributorAutorizzato update(RichiestaCreazioneContributorDTO contributorDTO, String username) {
        //TODO
        return null;
    }


    public ContributorAutorizzato save(ContributorAutorizzato contributorAutorizzato) {
        contributorAutorizzato = repository.save(contributorAutorizzato);
        return contributorAutorizzato;
    }


    public void deleteAll() {
        repository.deleteAll();
    }


    public List<ContributorAutorizzato> findByNomeComune(String nomeComune) {
        return repository.findByComuneNome(nomeComune);
    }


    @Override
    public Itinerario aggiungiItinerario(Itinerario itinerario) {
        return itinerarioServiceImpl.creaItinerario(itinerario);
    }

    @Override
    @Transactional
    public boolean aggiungiTappaItinerario(Itinerario itinerario, PuntoInteresse puntoInteresse) {
        return itinerarioServiceImpl.aggiungiTappa(itinerario, puntoInteresse);
    }

    @Override
    @Transactional
    public void aggiungiTappaItinerario(Itinerario itinerario, PuntoInteresse... puntiInteresse) {
        for (PuntoInteresse puntoInteresse : puntiInteresse) {
            aggiungiTappaItinerario(itinerario, puntoInteresse);
        }
    }

    @Override
    public void modificaScadenza(PuntoInteresse puntoInteresse, LocalDate expireDate) {
        puntoInteresse.setExpireDate(expireDate);
    }

    @Override
    public List<ContributorAutorizzato> getAll() {
        return repository.findAll();
    }
}
