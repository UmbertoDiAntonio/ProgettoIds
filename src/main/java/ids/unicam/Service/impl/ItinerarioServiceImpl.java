package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.ItinerarioRepository;
import ids.unicam.Service.ContributorService;
import ids.unicam.Service.ItinerarioService;
import ids.unicam.Service.PoiService;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.Comune;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import jakarta.transaction.Transactional;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

import static ids.unicam.Main.logger;

@Service
@Transactional
public class ItinerarioServiceImpl implements ItinerarioService {
    private final ItinerarioRepository repository;
    private final PoiService poiService;
    private final ContributorService contributorService;

    @Autowired
    public ItinerarioServiceImpl(ItinerarioRepository repository, PoiService poiService, ContributorService contributorService) {
        this.repository = repository;
        this.poiService = poiService;
        this.contributorService = contributorService;
    }


    @Override
    public void deleteById(int id) {
        repository.deleteById(id);
    }

    @Override
    public @NotNull List<Itinerario> find(@Nullable Predicate<Itinerario> predicate) {
        if (predicate == null)
            return getAll();
        List<Itinerario> list = new ArrayList<>();
        for (Itinerario itinerario : getAll())
            if (predicate.test(itinerario))
                list.add(itinerario);
        return Collections.unmodifiableList(list);
    }

    @Override
    public @NotNull Itinerario save(@NotNull Itinerario itinerario) {
        return repository.save(itinerario);
    }


    @Override
    @Transactional
    public void aggiungiTappa(@NotNull String usernameContributor, int idItinerario, int idPuntoInteresse) throws IllegalArgumentException, FuoriComuneException {
        Optional<Contributor> oContributor = contributorService.getByUsername(usernameContributor);
        if (oContributor.isPresent()) {
            Contributor contributor = oContributor.get();
            Optional<PuntoInteresse> oPoi = poiService.findById(idPuntoInteresse);
            if (oPoi.isPresent()) {
                PuntoInteresse puntoInteresse = oPoi.get();
                Optional<Itinerario> oItinerario = getById(idItinerario);
                if (oItinerario.isPresent()) {
                    Itinerario itinerario = oItinerario.get();
                    if (contributor.getComune().getNome().equals(itinerario.getComune().getNome())) {
                        if (itinerario.getComune().equals(puntoInteresse.getComune())) {
                            if (!itinerario.getPercorso().contains(puntoInteresse)) {
                                poiService.save(puntoInteresse);
                                itinerario.aggiungiTappa(puntoInteresse);
                                save(itinerario);
                            } else {
                                logger.warn("il punto di interesse e' gia' una tappa dell'itinerario");
                                throw new IllegalArgumentException("il punto di interesse e' gia' una tappa dell'itinerario");
                            }
                        } else {
                            throw new FuoriComuneException("La tappa non fa parte del comune dell'itinerario");
                        }
                    } else {
                        throw new FuoriComuneException("il contributor non fa parte del comune dell'itinerario");
                    }
                } else {
                    logger.warn("id Itinerario non valido");
                    throw new IllegalArgumentException("id itinerario non valido");
                }
            } else {
                logger.warn("id Punto Interesse non valido");
                throw new IllegalArgumentException("id Punto Interesse non valido");
            }
        } else {
            logger.warn("username non e' non valido");
            throw new IllegalArgumentException("username non e' non valido");
        }
    }

    @Override
    public void aggiungiTappa(@NotNull String usernameContributor, int idItinerario, int... idPuntiInteresse) throws IllegalArgumentException, FuoriComuneException {
        Optional<Itinerario> oItinerario = getById(idItinerario);
        if (oItinerario.isPresent()) {
            Itinerario itinerario = oItinerario.get();
            for (int idPuntoInteresse : idPuntiInteresse) {
                aggiungiTappa(usernameContributor, itinerario.getId(), idPuntoInteresse);
            }
        }
    }


    @Override
    public void rimuoviTappa(@NotNull String usernameContributor, int idItinerario, int idPuntoInteresse) throws
            IllegalArgumentException, FuoriComuneException {
        Optional<Contributor> oContributor = contributorService.getByUsername(usernameContributor);
        if (oContributor.isPresent()) {
            Contributor contributor = oContributor.get();
            Optional<Itinerario> oItinerario = getById(idItinerario);
            if (oItinerario.isPresent()) {
                Itinerario itinerario = oItinerario.get();
                if (contributor.getComune().equals(itinerario.getComune())) {
                    Optional<PuntoInteresse> oPoi = poiService.getById(idPuntoInteresse);
                    if (oPoi.isPresent()) {
                        PuntoInteresse puntoInteresse = oPoi.get();
                        itinerario.rimuoviTappa(puntoInteresse);
                        save(itinerario);
                    } else {
                        logger.warn("id Punto Interesse non valido");
                        throw new IllegalArgumentException("id Punto Interesse non valido");
                    }
                } else {
                    throw new FuoriComuneException(contributor + " non può rimuovere tappe da itinerari esterni al suo comune");
                }
            } else {
                logger.warn("id Itinerario non valido");
                throw new IllegalArgumentException("id itinerario non valido");
            }
        } else {
            logger.warn("username Contributor non valido");
            throw new IllegalArgumentException("username Contributor non valido");
        }
    }


    @Override
    public @NotNull Itinerario creaItinerario(@NotNull String usernameCreatore, @NotNull String nomeItinerario) throws IllegalArgumentException {
        Optional<Itinerario> oItinerario = getByNome(nomeItinerario);
        Optional<Contributor> oContributor = contributorService.getByUsername(usernameCreatore);
        if (oContributor.isPresent()) {
            Comune comune = oContributor.get().getComune();
            if (oItinerario.isPresent()) {
                Itinerario itinerario = oItinerario.get();
                if (itinerario.getComune().getNome().equals(comune.getNome())) {
                    logger.error("Esiste già un itinerario con questo nome nel comune");
                    throw new IllegalArgumentException("Esiste già un itinerario con questo nome nel comune");
                }
            }
            return save(new Itinerario(nomeItinerario, comune));
        } else {
            logger.warn("Il contributor non e' valido");
            throw new IllegalArgumentException("Il contributor non e' valido");
        }
    }


    @Override
    public @NotNull List<PuntoInteresse> getTappe(int idItinerario) throws IllegalArgumentException {
        Optional<Itinerario> oItinerario = getById(idItinerario);
        if (oItinerario.isPresent()) {
            Itinerario itinerario = oItinerario.get();
            return Collections.unmodifiableList(itinerario.getPercorso());
        }
        logger.warn("id itinerario non valido");
        throw new IllegalArgumentException("id itinerario non valido");

    }

    @Override
    public @NotNull Optional<Itinerario> getByNome(@NotNull String nome) {
        return repository.findByNome(nome);
    }

    @Override
    public @NotNull Optional<Itinerario> getById(int id) {
        return repository.findById(id);
    }

    @Override
    public @NotNull List<Itinerario> getAll() {
        return repository.findAll();
    }


}
