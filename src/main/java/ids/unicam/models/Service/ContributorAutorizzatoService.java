package ids.unicam.models.Service;

import ids.unicam.models.Repository.ContributorAutorizzatoRepository;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.MaterialeGenerico;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Stato;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class ContributorAutorizzatoService{
    private final ContributorAutorizzatoRepository repository;
    private final PoiService poiService;
    private final ItinerarioService itinerarioService;
    private final MaterialeService materialeService;
    @Autowired
    public ContributorAutorizzatoService(ContributorAutorizzatoRepository repository, PoiService poiService, ItinerarioService itinerarioService, MaterialeService materialeService) {
        this.repository = repository;
        this.poiService = poiService;
        this.itinerarioService = itinerarioService;
        this.materialeService = materialeService;
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

    public PuntoInteresse aggiungiPuntoInteresse(Contributor contributor, PuntoInteresse puntoInteresse){
        puntoInteresse.setStato(Stato.APPROVED);
        return poiService.save(puntoInteresse);
    }

    public Itinerario aggiungiItinerario(Itinerario itinerario){
        return  itinerarioService.save(itinerario);
    }

    //TODO potrebbe essere inutile, eliminare
    public MaterialeGenerico aggiungiMateriale(ContributorAutorizzato contributorAutorizzato, PuntoInteresse puntoInteresse, MaterialeGenerico materialeGenerico) {
        return poiService.creaMateriale(contributorAutorizzato,puntoInteresse,materialeGenerico);
    }
}
