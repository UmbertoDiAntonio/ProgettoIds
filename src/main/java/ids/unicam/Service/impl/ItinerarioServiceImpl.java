package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.ItinerarioRepository;
import ids.unicam.Service.ItinerarioService;
import ids.unicam.models.Comune;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.PutMapping;

import java.util.List;
import java.util.Optional;

import static ids.unicam.Main.logger;

@Service
@Transactional
public class ItinerarioServiceImpl implements ItinerarioService {
    private final ItinerarioRepository repository;
    private final PoiServiceImpl poiServiceImpl;

    @Autowired
    public ItinerarioServiceImpl(ItinerarioRepository repository, PoiServiceImpl poiServiceImpl) {
        this.repository = repository;
        this.poiServiceImpl = poiServiceImpl;
    }


    @Override
    public void deleteById(int id) {
        repository.deleteById(id);
    }

    @Override
    public Itinerario update(Itinerario itinerario, int id) {
        //TODO
        return null;
    }


    public Itinerario save(Itinerario itinerario) {
        return repository.save(itinerario);
    }

    @Override
    @Transactional
    public boolean aggiungiTappa(String usernameContributor, Integer idItinerario, Integer idPuntoInteresse) {
        Optional<PuntoInteresse> oPoi = poiServiceImpl.findById(idPuntoInteresse);
        if (oPoi.isPresent()) {
            PuntoInteresse puntoInteresse = oPoi.get();
            Optional<Itinerario> oItinerario = getById(idItinerario);
            if (oItinerario.isPresent()) {
                Itinerario itinerario = oItinerario.get();
                if (itinerario.getComune().equals(puntoInteresse.getComune())) {
                    poiServiceImpl.save(puntoInteresse);
                    itinerario.getPercorso().add(puntoInteresse);
                    save(itinerario);
                    return true;
                }
                return false;
            }
            logger.error("id Itinerario non valido");
            throw new IllegalArgumentException("id itinerario non valido");
        }
        logger.error("id Punto Interesse non valido");
        throw new IllegalArgumentException("id Punto Interesse non valido");
    }

    @Override
    public void aggiungiTappa(String usernameContributor, Integer idItinerario, Integer... idPuntiInteresse) {
        Optional<Itinerario> oItinerario = getById(idItinerario);
        if (oItinerario.isPresent()) {
            Itinerario itinerario = oItinerario.get();
            for (Integer idPuntoInteresse : idPuntiInteresse) {
                aggiungiTappa(usernameContributor, itinerario.getId(), idPuntoInteresse);
            }
        }
    }


    @Override
    public void rimuoviTappa(Itinerario itinerario, PuntoInteresse puntoInteresse) {
        itinerario.getPercorso().remove(puntoInteresse);
        save(itinerario);
    }

    public void deleteAll() {
        repository.deleteAll();
    }

    public List<Itinerario> findAllByComune(Comune comune) {
        return repository.findAllByComune(comune);
    }

    public int getNumeroTappe(Itinerario itinerario) {
        return repository.countNumeroTappeItinerario(itinerario.getId());
    }

    @Override
    public Itinerario creaItinerario(Itinerario itinerario) {
        for (PuntoInteresse puntoInteresse : itinerario.getPercorso()) {
            if (!itinerario.getComune().verificaCoordinateComune(puntoInteresse.getPt()) || !(puntoInteresse.getStato().asBoolean() != null && (puntoInteresse.getStato().asBoolean()))) {
                logger.error("Non si possono creare Itinerari con punti non approvati");
                throw new IllegalArgumentException("Non si possono creare Itinerari con punti non approvati");
            }
        }
        return save(itinerario);
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
