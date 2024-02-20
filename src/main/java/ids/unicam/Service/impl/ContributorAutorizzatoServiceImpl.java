package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.ContributorAutorizzatoRepository;
import ids.unicam.models.Comune;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.ContributorAutorizzato;
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
public class ContributorAutorizzatoServiceImpl {
    private final ContributorAutorizzatoRepository repository;
    private final PoiServiceImpl poiServiceImpl;
    private final ItinerarioServiceImpl itinerarioServiceImpl;

    @Autowired
    public ContributorAutorizzatoServiceImpl(ContributorAutorizzatoRepository repository, PoiServiceImpl poiServiceImpl, ItinerarioServiceImpl itinerarioServiceImpl) {
        this.repository = repository;
        this.poiServiceImpl = poiServiceImpl;
        this.itinerarioServiceImpl = itinerarioServiceImpl;
    }
    public void deleteById(int id) {
        repository.deleteById(id);
    }


    public ContributorAutorizzato save(ContributorAutorizzato contributorAutorizzato) {
        contributorAutorizzato = repository.save(contributorAutorizzato);
        return contributorAutorizzato;
    }



    public Optional<ContributorAutorizzato> findById(int id) {
        return repository.findById(id);
    }


    public List<ContributorAutorizzato> findAll() {
        return repository.findAll();
    }

    public ContributorAutorizzato getLast() {
        return repository.findAll().getLast();
    }

    public ContributorAutorizzato getFirst() {
        return repository.findAll().getFirst();
    }


    public void deleteAll() {
        repository.deleteAll();
    }


    public List<ContributorAutorizzato> findByNomeComune(String nomeComune) {
        return repository.findByComuneNome(nomeComune);
    }

    public void aggiungiPuntoInteresse(Contributor contributor, PuntoInteresse puntoInteresse){
        if (!contributor.getComune().equals(puntoInteresse.getComune())) {
            logger.error(contributor.getNome() + " non puo' creare punti di interesse fuori dal suo comune");
            throw new UnsupportedOperationException(contributor + " non può creare punti di interesse fuori dal suo comune");
        }
        puntoInteresse.setStato(Stato.APPROVATO);
        poiServiceImpl.save(puntoInteresse);
    }

    public Itinerario aggiungiItinerario(Comune comune, String nome, PuntoInteresse... puntiInteresse){
        return  itinerarioServiceImpl.creaItinerario(comune,nome,puntiInteresse);
    }

    @Transactional
    public boolean aggiungiTappaItinerario(Itinerario itinerario,PuntoInteresse puntoInteresse){
        return itinerarioServiceImpl.aggiungiTappa(itinerario,puntoInteresse);
    }
    @Transactional
    public void aggiungiTappaItinerario(Itinerario itinerario,PuntoInteresse... puntiInteresse){
        for(PuntoInteresse puntoInteresse:puntiInteresse) {
            aggiungiTappaItinerario(itinerario, puntoInteresse);
        }
    }

    public void modificaScadenza(PuntoInteresse puntoInteresse, LocalDate expireDate) {
        puntoInteresse.setExpireDate(expireDate);
    }
}
