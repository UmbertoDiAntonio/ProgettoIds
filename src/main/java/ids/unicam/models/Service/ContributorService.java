package ids.unicam.models.Service;

import ids.unicam.Comune;
import ids.unicam.models.Repository.ContributorRepository;
import ids.unicam.models.attori.Contributor;
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
public class ContributorService {
    private final ContributorRepository repository;
    private final PoiService poiService;
    private final ItinerarioService itinerarioService;

    @Autowired
    public ContributorService(ContributorRepository repository, PoiService poiService, ItinerarioService itinerarioService) {
        this.repository = repository;
        this.poiService = poiService;
        this.itinerarioService = itinerarioService;
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

    public PuntoInteresse aggiungiPuntoInteresse(Contributor contributor, PuntoInteresse puntoInteresse) {
        if (!contributor.getComune().equals(puntoInteresse.getComune())) {
            logger.error(contributor.getNome() + " non puo' creare punti di interesse fuori dal suo comune");
            throw new UnsupportedOperationException(contributor + " non può creare punti di interesse fuori dal suo comune");
        }
        puntoInteresse.setStato(Stato.NOT_APPROVED);
        return poiService.save(puntoInteresse);
    }

    public Itinerario aggiungiItinerario(Comune comune, String nome, PuntoInteresse... puntoInteresse) {
        return itinerarioService.creaItinerario(comune, nome, puntoInteresse);
    }

    @Transactional
    public boolean aggiungiTappaItinerario(Itinerario itinerario, PuntoInteresse puntoInteresse) {
        return itinerarioService.aggiungiTappa(itinerario, puntoInteresse);
    }

    public void modificaScandenza(PuntoInteresse puntoInteresse, LocalDate expireDate) {
        puntoInteresse.setExpireDate(expireDate);
        poiService.save(puntoInteresse);
    }
}
