package ids.unicam.controller;

import ids.unicam.Service.TuristaService;
import ids.unicam.models.DTO.PuntoInteresseDTO;
import ids.unicam.models.DTO.RichiestaCreazioneTagDTO;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Taggable;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Optional;

@RequestMapping("/Turista")
@RestController
public class TuristaController  {

    private final TuristaService turistaService;

    public TuristaController(TuristaService turistaService) {
        this.turistaService = turistaService;
    }

    @GetMapping("/{tagDTO}")
    public List<Taggable> getByTag(@PathVariable RichiestaCreazioneTagDTO tagDTO){
        return turistaService.findByTag(tagDTO);
    }

    @GetMapping("/report")
    public void report(@RequestParam PuntoInteresseDTO poiDTO, @RequestParam String messaggio){
        turistaService.report(poiDTO, messaggio);
    }

    @GetMapping("/accedi")
    public Optional<TuristaAutenticato> accedi(@RequestParam String username, @RequestParam String password){
        return turistaService.accedi(username,password);
    }
}
