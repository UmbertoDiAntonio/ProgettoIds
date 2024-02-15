package ids.unicam.models.Service;

import ids.unicam.models.Taggable;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.models.contenuti.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class TuristaService {

    private final PoiService poiService;
    private final TuristaAutenticatoService turistaAutenticatoService;

    @Autowired
    public TuristaService(PoiService poiService, TuristaAutenticatoService turistaAutenticatoService) {
        this.poiService = poiService;
        this.turistaAutenticatoService = turistaAutenticatoService;
    }

    public List<Taggable> findByTag(Tag tag){
       return poiService.findByTag(tag);
    }

    public void report(PuntoInteresse puntoInteresse){
        //TODO
    }

    public Optional<TuristaAutenticato> accedi(String username, String password){
        if(turistaAutenticatoService.verificaPassword(password, username))
            return turistaAutenticatoService.findTuristaByUsername(username);
        return Optional.empty();
    }
}