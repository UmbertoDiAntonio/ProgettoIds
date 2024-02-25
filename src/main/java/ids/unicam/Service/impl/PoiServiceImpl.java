package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.PoiRepository;
import ids.unicam.Service.PoiService;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import ids.unicam.models.contenuti.puntiInteresse.Tag;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import static ids.unicam.Main.logger;

@Service
public class PoiServiceImpl implements PoiService {
    private final PoiRepository repository;
    private final MaterialeServiceImpl materialeServiceImpl;
    private final TagServiceImpl tagServiceImpl;
    private final ContributorServiceImpl contributorService;

    @Autowired
    public PoiServiceImpl(PoiRepository repository, MaterialeServiceImpl materialeServiceImpl, TagServiceImpl tagServiceImpl, ContributorServiceImpl contributorService) {
        this.repository = repository;
        this.materialeServiceImpl = materialeServiceImpl;
        this.tagServiceImpl = tagServiceImpl;
        this.contributorService = contributorService;
    }


    public void deleteById(int id) {
        repository.deleteById(id);
    }

    @Transactional
    @Override
    public void modificaScadenza(String usernameContributor, Integer idPuntoInteresse, LocalDate expireDate) throws IllegalArgumentException {
        Optional<Contributor> oContributor = contributorService.getById(usernameContributor);
        if (oContributor.isPresent()) {
            Contributor contributor = oContributor.get();
            Optional<PuntoInteresse> oPoi = findById(idPuntoInteresse);
            if (oPoi.isPresent()) {
                PuntoInteresse puntoInteresse = oPoi.get();
                if (contributor.getComune().equals(puntoInteresse.getComune())) {
                    puntoInteresse.setExpireDate(expireDate);
                    if (expireDate.isAfter(LocalDate.now()))
                        save(puntoInteresse);
                } else {
                    logger.error("punto di interesse fuori dal comune di appartenenza del contributor");
                    throw new IllegalArgumentException("punto di interesse fuori dal comune di appartenenza del contributor");
                }
            } else {
                logger.error("id del punto di interesse non esiste");
                throw new IllegalArgumentException("id del punto di interesse non esiste");
            }
        } else {
            logger.error("username del contributor non esiste");
            throw new IllegalArgumentException("username del contributor non esiste");
        }
    }

    @Override
    public Stato getStato(int idPunto) throws IllegalArgumentException {
        Optional<Stato> oStato = repository.getStatoById(idPunto);
        if(oStato.isEmpty())
            throw new IllegalArgumentException("id punto non valido");
        return oStato.get();
    }

    @Transactional
    @Override
    public PuntoInteresse creaPuntoInteresse(PuntoInteresse puntoInteresse) throws FuoriComuneException {
        if (!puntoInteresse.getCreatore().getComune().verificaCoordinateComune(puntoInteresse.getPt())) {
            logger.error("Non si possono creare punti di interesse fuori dal comune");
            throw new FuoriComuneException("Posizione Punto di Interesse Fuori dall'area del comune");
        }
        return save(puntoInteresse);
    }


    @Transactional
    @Override
    public void eliminaPuntoInteresse(int idPuntoInteresse) {
        // Elimina il PuntoInteresse dal database
        repository.deleteById(idPuntoInteresse);
/*TODO
        // Rileva l'eliminazione e aggiorna le liste di preferiti dei turisti
        List<TuristaAutenticato> turisti = turistaAutenticatoServiceImpl.findTuristiConPreferiti();
        for (TuristaAutenticato turista : turisti) {
            turistaAutenticatoServiceImpl.rimuoviPreferito(turista.getUsername(), idPuntoInteresse);

        }

 */
    }

    public LocalDate getScadenza(int idPunto) throws IllegalArgumentException {
        Optional<LocalDate> oScadenza = repository.getExpireDateById(idPunto);
        if(oScadenza.isEmpty())
            throw new IllegalArgumentException("id punto non valido");
        return oScadenza.get();
    }

    @Transactional
    @Override
    public void aggiungiMateriale(TuristaAutenticato turistaAutenticato, PuntoInteresse puntoInteresse, MaterialeGenerico materialeGenerico) {
        if (turistaAutenticato instanceof Contributor contributor) {
            if (!contributor.getComune().equals(puntoInteresse.getComune())) {
                logger.error("il contributor cerca di caricare il materiale fuori dal suo comune");
                throw new IllegalStateException("il contributor cerca di caricare il materiale fuori dal suo comune");
            }
        }
        if (Boolean.FALSE.equals(puntoInteresse.getStato().asBoolean())) {
            logger.error("il contributor cerca di caricare il materiale su un punto di interesse non approvato");
            throw new IllegalStateException("il contributor cerca di caricare il materiale su un punto di interesse non approvato");
        }
        if (turistaAutenticato instanceof ContributorAutorizzato)
            materialeGenerico.setStato(Stato.APPROVATO);
        puntoInteresse.addMateriale(materialeGenerico);
        materialeServiceImpl.save(materialeGenerico);
        save(puntoInteresse);
    }


    @Transactional
    public PuntoInteresse save(PuntoInteresse puntoInteresse) {
        return repository.save(puntoInteresse);
    }


    public Optional<PuntoInteresse> findById(int id) {
        return repository.findById(id);
    }


    @Transactional
    @Override
    public List<PuntoInteresse> findActive() {
        return repository.findAll().stream()
                .filter(puntoInteresse -> !puntoInteresse.isExpired() || Boolean.TRUE.equals(puntoInteresse.getStato().asBoolean()))
                .peek(puntoInteresse -> {
                    if (puntoInteresse.isExpired()) {
                        repository.deleteById(puntoInteresse.getId());
                    }
                })
                .collect(Collectors.toList());
    }


    public void deleteAll() {
        repository.deleteAll();
    }


    @Transactional
    @Override
    public void aggiungiTag(PuntoInteresse puntoInteresse, Tag tag) {
        if (tagServiceImpl.haveTag(puntoInteresse, tag)) {
            logger.warn("Tag già aggiunto");
            return;
        }
        if (!puntoInteresse.isExpired())
            tagServiceImpl.aggiungiTag(puntoInteresse, tag);

        save(puntoInteresse);
    }

    @Override
    public List<Taggable> findByTag(String tag) {
        return repository.findByTagsValoreContaining(tag);
    }

    @Override
    public List<Tag> getTags(PuntoInteresse puntoInteresse) {
        return repository.getTags(puntoInteresse.getId());
    }

    @Override
    public Optional<PuntoInteresse> getById(int id) {
        return repository.findById(id);
    }

    public List<MaterialeGenerico> getMaterialiPoi(Integer idPunto) {
        return repository.getMateriali(idPunto);
    }

    public List<PuntoInteresse> findAll() {
        return repository.findAll();
    }
}
