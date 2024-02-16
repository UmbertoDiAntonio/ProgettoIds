package ids.unicam.models.Service;

import ids.unicam.Comune;
import ids.unicam.models.Repository.ContributorAutorizzatoRepository;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Stato;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

import static ids.unicam.Main.logger;

@Service
public class ContributorAutorizzatoService{
    private final ContributorAutorizzatoRepository repository;
    private final PoiService poiService;
    private final ItinerarioService itinerarioService;

    @Autowired
    public ContributorAutorizzatoService(ContributorAutorizzatoRepository repository, PoiService poiService, ItinerarioService itinerarioService) {
        this.repository = repository;
        this.poiService = poiService;
        this.itinerarioService = itinerarioService;
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
        if (contributor.getComune().equals(puntoInteresse.getComune())) {
            logger.error(contributor + " non può creare punti di interesse fuori dal suo comune");
            throw new UnsupportedOperationException(contributor + " non può creare punti di interesse fuori dal suo comune");
        }
        puntoInteresse.setStato(Stato.APPROVED);
        return poiService.save(puntoInteresse);
    }

    public Itinerario aggiungiItinerario(Comune comune, String nome, PuntoInteresse... puntoInteresses){
        return  itinerarioService.creaItinerario(comune,nome,puntoInteresses);
    }

    @Transactional
    public boolean aggiungiTappaItinerario(Itinerario itinerario,PuntoInteresse puntoInteresse){
        return itinerarioService.aggiungiTappa(itinerario,puntoInteresse);
    }
    @Transactional
    public void aggiungiTappaItinerario(Itinerario itinerario,PuntoInteresse... puntiInteresse){
        for(PuntoInteresse puntoInteresse:puntiInteresse) {
            aggiungiTappaItinerario(itinerario, puntoInteresse);
        }
    }

    public void modificaScandenza(PuntoInteresse puntoInteresse, LocalDate expireDate) {
        puntoInteresse.setExpireDate(expireDate);
    }
}
