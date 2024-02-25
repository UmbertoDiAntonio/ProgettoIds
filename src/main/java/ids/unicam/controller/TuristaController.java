package ids.unicam.controller;

import ids.unicam.Service.TuristaService;
import ids.unicam.models.DTO.PuntoInteresseDTO;
import ids.unicam.models.contenuti.Taggable;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RequestMapping("/Turista")
@RestController
public class TuristaController  {

    private final TuristaService turistaService;

    public TuristaController(TuristaService turistaService) {
        this.turistaService = turistaService;
    }

    @GetMapping("/{tag}")
    public List<Taggable> getByTag(@PathVariable String tag){
        return turistaService.findByTag(tag);
    }

    @GetMapping("/report")
    public void report(@RequestParam PuntoInteresseDTO poiDTO, @RequestParam String messaggio){
        turistaService.report(poiDTO, messaggio);
    }

}
