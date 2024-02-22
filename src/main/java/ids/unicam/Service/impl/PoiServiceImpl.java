package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.PoiRepository;
import ids.unicam.Service.PoiService;
import ids.unicam.models.DTO.RichiestaCreazioneTagDTO;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import ids.unicam.models.contenuti.puntiInteresse.Tag;
import jakarta.transaction.Transactional;
import org.jetbrains.annotations.Nullable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import static ids.unicam.Main.logger;

@Service
public class PoiServiceImpl implements PoiService {
    private final PoiRepository repository;
    private final TuristaAutenticatoServiceImpl turistaAutenticatoServiceImpl;
    private final MaterialeServiceImpl materialeServiceImpl;
    private final TagServiceImpl tagServiceImpl;

    @Autowired
    public PoiServiceImpl(PoiRepository repository, TuristaAutenticatoServiceImpl turistaAutenticatoServiceImpl, MaterialeServiceImpl materialeServiceImpl, TagServiceImpl tagServiceImpl) {
        this.repository = repository;
        this.turistaAutenticatoServiceImpl = turistaAutenticatoServiceImpl;
        this.materialeServiceImpl = materialeServiceImpl;
        this.tagServiceImpl = tagServiceImpl;
    }


    public void deleteById(int id) {
        repository.deleteById(id);
    }

    @Transactional
    @Override
    public @Nullable PuntoInteresse creaPuntoInteresse(PuntoInteresse puntoInteresse) {
        return save(puntoInteresse);
    }


    @Transactional
    @Override
    public void eliminaPuntoInteresse(int idPuntoInteresse) {
        // Elimina il PuntoInteresse dal database
        repository.deleteById(idPuntoInteresse);

        // Rileva l'eliminazione e aggiorna le liste di preferiti dei turisti
        List<TuristaAutenticato> turisti = turistaAutenticatoServiceImpl.findTuristiConPreferiti();
        for (TuristaAutenticato turista : turisti) {
            turistaAutenticatoServiceImpl.rimuoviPreferito(turista, idPuntoInteresse);

        }
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
    public List<Taggable> findByTag(RichiestaCreazioneTagDTO tagDTO) {
        return repository.findByTagsValoreContaining(tagDTO.getValore());
    }

    @Override
    public List<Tag> getTags(PuntoInteresse puntoInteresse) {
        return repository.getTags(puntoInteresse.getId());
    }

    @Override
    public Optional<PuntoInteresse> getById(int id) {
        return repository.findById(id);
    }

    @Override
    public PuntoInteresse update(PuntoInteresse puntoInteresse, int id) {
        //TODO
        return null;
    }

    public List<MaterialeGenerico> getMaterialiPoi(PuntoInteresse contenutoGenerico) {
        return repository.getMateriali(contenutoGenerico.getId());
    }

    public List<PuntoInteresse> findAll() {
        return repository.findAll();
    }
}
