package ids.unicam.models.Service;

import ids.unicam.Comune;
import ids.unicam.controller.ComuneController;
import ids.unicam.models.Repository.ContributorRepository;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.PuntoInteresse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class ContributorService {
    private final ContributorRepository repository;
    private final PoiService poiService;
    private final ItinerarioService itinerarioService;
    private final MaterialeService materialeService;

    @Autowired
    public ContributorService(ContributorRepository repository, PoiService poiService, ItinerarioService itinerarioService, MaterialeService materialeService) {
        this.repository = repository;
        this.poiService = poiService;
        this.itinerarioService = itinerarioService;
        this.materialeService = materialeService;
    }

    public List<Contributor> findByNomeComune(String nomeComune) {
        return repository.findByComuneNome(nomeComune);
    }

    public void deleteById(int id) {
        repository.deleteById(id);
    }


    public Contributor save(Contributor contributor) {
        contributor = repository.save(contributor);
        return contributor;
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

    public PuntoInteresse aggiungiPuntoInteresse(Contributor contributor,PuntoInteresse puntoInteresse){
        return poiService.save(contributor,puntoInteresse);
    }

    public Itinerario aggiungiItinerario(Itinerario itinerario){
        return  itinerarioService.save(itinerario);
    }
    public boolean aggiungiTappaItinerario(Itinerario itinerario,PuntoInteresse puntoInteresse){
        return itinerarioService.aggiungiTappa(itinerario,puntoInteresse);
    }
}
