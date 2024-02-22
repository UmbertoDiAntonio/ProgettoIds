package ids.unicam.controller;

import ids.unicam.Service.TuristaService;
import ids.unicam.models.DTO.RichiestaCreazioneTagDTO;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import ids.unicam.models.contenuti.puntiInteresse.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RequestMapping("/Turista")
@RestController
public class TuristaController  {

    private TuristaService turistaService;


    public TuristaController(TuristaService turistaService) {
        this.turistaService = turistaService;
    }

    public List<Taggable> getByTag(RichiestaCreazioneTagDTO tagDTO){
        //TODO
        return null;
    }

    public void report(PuntoInteresse puntoInteresse, String messaggio){
        //TODO
    }

    public void accedi(String username, String password){
        //TODO
    }
}
