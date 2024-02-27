package ids.unicam.controller;

import ids.unicam.Service.PoiService;
import ids.unicam.Service.TuristaService;
import ids.unicam.models.DTO.PuntoInteresseDTO;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Optional;

@RequestMapping("/Turista")
@RestController
public class TuristaController  {
    private final PoiService poiService;
    private final TuristaService turistaService;

    public TuristaController(PoiService poiService, TuristaService turistaService) {
        this.poiService = poiService;
        this.turistaService = turistaService;
    }

    @GetMapping("/{tag}")
    public List<Taggable> getByTag(@PathVariable String tag){
        return turistaService.findByTag(tag);
    }

    @GetMapping("/report")
    public ResponseEntity<String> report(@RequestParam Integer idPunto, @RequestParam String messaggio){
        Optional<PuntoInteresse> oPunto = poiService.getById(idPunto);
        if(oPunto.isEmpty()){
            return ResponseEntity.ok("Punto non valido");
        }
        PuntoInteresse punto = oPunto.get();
        turistaService.report(new PuntoInteresseDTO(punto), messaggio);
        return ResponseEntity.ok("Punto segnalato");
    }

}
