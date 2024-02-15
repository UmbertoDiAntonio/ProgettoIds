package ids.unicam.models.Service;

import ids.unicam.models.Repository.PoiRepository;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.MaterialeGenerico;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.models.contenuti.Tag;
import ids.unicam.utilites.Stato;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;

import static ids.unicam.Main.logger;

@Service
public class PoiService {
    private final PoiRepository repository;
    private final TuristaAutenticatoService turistaAutenticatoService;
    private final MaterialeService materialeService;
    private final TagService tagService;

    @Autowired
    public PoiService(PoiRepository repository, TuristaAutenticatoService turistaAutenticatoService, MaterialeService materialeService, TagService tagService) {
        this.repository = repository;
        this.turistaAutenticatoService = turistaAutenticatoService;
        this.materialeService = materialeService;
        this.tagService = tagService;
    }


    public void deleteById(int id) {
        repository.deleteById(id);
    }

    @Transactional
    public void eliminaPuntoInteresse(int idPuntoInteresse) {
        // Elimina il PuntoInteresse dal database
        repository.deleteById(idPuntoInteresse);

        // Rileva l'eliminazione e aggiorna le liste di preferiti dei turisti
        List<TuristaAutenticato> turisti = turistaAutenticatoService.findTuristiConPreferiti();
        for (TuristaAutenticato turista : turisti) {
            turistaAutenticatoService.rimuoviPreferito(turista, idPuntoInteresse);

        }
    }

    public MaterialeGenerico creaMateriale(TuristaAutenticato turistaAutenticato, PuntoInteresse puntoInteresse, MaterialeGenerico materialeGenerico) {
        if (turistaAutenticato instanceof Contributor contributor) {
            if (!contributor.getComune().equals(puntoInteresse.getComune())) {
                logger.error("il contributor cerca di caricare il materiale fuori dal suo comune");
                throw new IllegalStateException("il contributor cerca di caricare il materiale fuori dal suo comune");
            }
        }
        if (!puntoInteresse.getStato().asBoolean()) {
            logger.error("il contributor cerca di caricare il materiale su un punto di interesse non approvato");
            throw new IllegalStateException("il contributor cerca di caricare il materiale su un punto di interesse non approvato");
        }
        if (turistaAutenticato instanceof ContributorAutorizzato)
            materialeGenerico.setStato(Stato.APPROVED);
        materialeGenerico.setIdProprietario(puntoInteresse.getId());
        return materialeService.save(materialeGenerico);
    }


    @Transactional
    public PuntoInteresse save(PuntoInteresse puntoInteresse) {
        return repository.save(puntoInteresse);
    }




    public Optional<PuntoInteresse> findById(int id) {
        return repository.findById(id);
    }


    public List<PuntoInteresse> findAll() {
        return repository.findAll();
    }

    public PuntoInteresse getLast() {
        return repository.findAll().getLast();
    }

    public PuntoInteresse getFirst() {
        return repository.findAll().getFirst();
    }


    public void deleteAll() {
        repository.deleteAll();
    }


    @Transactional
    public void aggiungiTag(PuntoInteresse puntoInteresse, Tag tag) {
        tagService.aggiungiTag(puntoInteresse,tag);
        System.out.println("A " + puntoInteresse.getTags());
        System.out.println("B "+getTags(puntoInteresse));
        if(getTags(puntoInteresse).contains(tag)){
            logger.warn("Tag già aggiunto");
            return;
        }
        if (!puntoInteresse.isExpired())
            puntoInteresse.getTags().add(tag);
        System.out.println("C " + puntoInteresse.getTags());
        System.out.println("D "+getTags(puntoInteresse));
        save(puntoInteresse);
        System.out.println("E " + puntoInteresse.getTags());
        System.out.println("F "+getTags(puntoInteresse));
    }

    public List<PuntoInteresse> findByTag(Tag tag) {
        return repository.findByTagsValoreContaining(tag.getValore());
    }

    public List<Tag> getTags(PuntoInteresse puntoInteresse) {
        return repository.getTags(puntoInteresse.getId());
    }
}
