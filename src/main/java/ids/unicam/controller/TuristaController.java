package ids.unicam.controller;

import ids.unicam.Service.TuristaService;
import ids.unicam.models.contenuti.Taggable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
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
    public ResponseEntity<?> getByTag(@PathVariable String tag){
        return ResponseEntity.ok(turistaService.findByTag(tag));
    }

    @GetMapping("/report")
    public ResponseEntity<?> report(@RequestParam int idPuntoInteresse, @RequestParam String messaggio){
        try {
            turistaService.report(idPuntoInteresse, messaggio);
            return ResponseEntity.ok("Il Punto di interesse con id '"+idPuntoInteresse+"' è stato segnalato.");
        }catch (IllegalArgumentException e){
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

}
