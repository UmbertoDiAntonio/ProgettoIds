package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.ItinerarioRepository;
import ids.unicam.Service.ItinerarioService;
import ids.unicam.models.Comune;
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
    private final PoiServiceImpl poiServiceImpl;

    @Autowired
    public ItinerarioServiceImpl(ItinerarioRepository repository, PoiServiceImpl poiServiceImpl) {
        this.repository = repository;
        this.poiServiceImpl = poiServiceImpl;
    }


    public void deleteById(int id) {
        repository.deleteById(id);
    }


    public Itinerario save(Itinerario itinerario) {
        return repository.save(itinerario);
    }

    @Override
    @Transactional
    public boolean aggiungiTappa(Itinerario itinerario, PuntoInteresse puntoInteresse) {
        if (itinerario.getComune().equals(puntoInteresse.getComune())) {
            poiServiceImpl.save(puntoInteresse);
            itinerario.aggiungiTappaPercorso(puntoInteresse);
            save(itinerario);
            return true;
        }
        return false;
    }

    @Override
    public void aggiungiTappa(Itinerario itinerario, PuntoInteresse... puntiInteresse) {
        for (PuntoInteresse puntoInteresse : puntiInteresse) {
            aggiungiTappa(itinerario, puntoInteresse);
        }
    }
    @Override
    public void rimuoviTappa(Itinerario itinerario, PuntoInteresse puntoInteresse) {
        itinerario.getPercorso().remove(puntoInteresse);
        save(itinerario);
    }

    public Optional<Itinerario> findById(int id) {
        return repository.findById(id);
    }


    public List<Itinerario> findAll() {
        return repository.findAll();
    }

    public Itinerario getLast() {
        return repository.findAll().getLast();
    }

    public Itinerario getFirst() {
        return repository.findAll().getFirst();
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
    public Itinerario creaItinerario(Comune comune, String nome, PuntoInteresse... puntiInteresse){
        for (PuntoInteresse puntoInteresse : puntiInteresse) {
            if (!comune.verificaCoordinateComune(puntoInteresse.getPt()) || !puntoInteresse.getStato().asBoolean()) {
                logger.error("Non si possono creare Itinerari con punti non approvati");
                throw new IllegalArgumentException("Non si possono creare Itinerari con punti non approvati");
            }
        }
        return save(new Itinerario(comune, nome, puntiInteresse));
    }
}
