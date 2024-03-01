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
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

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

    Itinerario save(Itinerario itinerario) {
        return repository.save(itinerario);
    }

    @Override
    @Transactional
    public boolean aggiungiTappa(String usernameContributor, int idItinerario, int idPuntoInteresse) throws IllegalArgumentException, FuoriComuneException {
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
                                itinerario.getPercorso().add(puntoInteresse);
                                save(itinerario);
                                return true;
                            } else {
                                logger.error("il punto di interesse e' gia' una tappa dell'itinerario");
                                throw new IllegalArgumentException("il punto di interesse e' gia' una tappa dell'itinerario");
                            }
                        } else {
                            logger.error("il punto di interesse e' fuori dal comune");
                            throw new IllegalArgumentException("il punto di interesse e' fuori dal comune");
                        }
                    } else {
                        throw new FuoriComuneException("il contributor non fa parte del comune dell'itinerario");
                    }
                } else {
                    logger.error("id Itinerario non valido");
                    throw new IllegalArgumentException("id itinerario non valido");
                }
            } else {
                logger.error("id Punto Interesse non valido");
                throw new IllegalArgumentException("id Punto Interesse non valido");
            }
        } else {
            logger.error("username non e' non valido");
            throw new IllegalArgumentException("username non e' non valido");
        }
    }

    @Override
    public void aggiungiTappa(String usernameContributor, int idItinerario, int... idPuntiInteresse) throws
            FuoriComuneException {
        Optional<Itinerario> oItinerario = getById(idItinerario);
        if (oItinerario.isPresent()) {
            Itinerario itinerario = oItinerario.get();
            for (int idPuntoInteresse : idPuntiInteresse) {
                aggiungiTappa(usernameContributor, itinerario.getId(), idPuntoInteresse);
            }
        }
    }


    @Override
    public void rimuoviTappa(String usernameContributor, int idItinerario, int idPuntoInteresse) throws
            IllegalArgumentException {
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
                        itinerario.getPercorso().remove(puntoInteresse);
                        save(itinerario);
                    } else {
                        logger.error("id Punto Interesse non valido");
                        throw new IllegalArgumentException("id Punto Interesse non valido");
                    }
                } else {
                    logger.warn(contributor + " non può rimuovere tappe da itinerari esterni al suo comune");
                    throw new UnsupportedOperationException(contributor + " non può rimuovere tappe da itinerari esterni al suo comune");
                }
            } else {
                logger.error("id Itinerario non valido");
                throw new IllegalArgumentException("id itinerario non valido");
            }
        } else {
            logger.error("username Contributor non valido");
            throw new IllegalArgumentException("username Contributor non valido");
        }
    }


    public List<Itinerario> findAllByComune(Comune comune) {
        return repository.findAllByComune(comune);
    }

    public int getNumeroTappe(Itinerario itinerario) {
        return repository.countNumeroTappeItinerario(itinerario.getId());
    }

    @Override
    public Itinerario creaItinerario(String usernameCreatore, String nomeItinerario) throws
            IllegalArgumentException {
        Optional<Itinerario> oItinerario = getByNome(nomeItinerario);
        Optional<Contributor> oContributor = contributorService.getByUsername(usernameCreatore);
        if (oContributor.isPresent()) {
            Comune comune = oContributor.get().getComune();
            if (oItinerario.isPresent()) {
                Itinerario itinerario = oItinerario.get();
                if (itinerario.getComune().getNome().equals(comune.getNome())) {
                    logger.error("L'itinerario esiste gia' nel comune");
                    throw new IllegalArgumentException("L'itinerario esiste gia' nel comune");
                }
            }

            return save(new Itinerario(nomeItinerario, comune));
        } else {
            logger.error("Il contributor non e' valido");
            throw new IllegalArgumentException("Il contributor non e' valido");
        }
    }

    public Optional<Itinerario> getByNome(String nome) {
        return repository.findByNome(nome);
    }

    @Override
    public Optional<Itinerario> getById(int id) {
        return repository.findById(id);
    }

    @Override
    public List<Itinerario> getAll() {
        return repository.findAll();
    }


}
