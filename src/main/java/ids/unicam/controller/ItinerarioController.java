package ids.unicam.controller;

import ids.unicam.Service.ItinerarioService;
import ids.unicam.exception.FuoriComuneException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/Itinerario")
public class ItinerarioController {

    private final ItinerarioService itinerarioService;

    public ItinerarioController(ItinerarioService itinerarioService) {
        this.itinerarioService = itinerarioService;
    }


    @GetMapping("/getAll")
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(itinerarioService.getAll());
    }

    @GetMapping("/{id}")
    public ResponseEntity<?> getById(@PathVariable Integer id) {
        return ResponseEntity.ok(itinerarioService.getById(id));
    }

    @PostMapping("/crea")
    public ResponseEntity<?> create(@RequestParam String nomeCreatore, @RequestParam String nomeItinerario) {
        try {
            return ResponseEntity.ok(itinerarioService.creaItinerario(nomeCreatore, nomeItinerario));
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<?> delete(@PathVariable Integer id) {
        itinerarioService.deleteById(id);
        return ResponseEntity.ok("L'itinerario '" + id + "' e' stato eliminato.");
    }

    @PutMapping("/aggiungiTappa")
    public ResponseEntity<?> aggiungiTappaItinerario(@RequestParam String usernameContributor, @RequestParam Integer idItinerario, @RequestParam Integer idTappa) {
        try {
            return ResponseEntity.ok(itinerarioService.aggiungiTappa(usernameContributor, idItinerario, idTappa));
        } catch (IllegalArgumentException | FuoriComuneException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);

        }
    }

    @PutMapping("/rimuoviTappa")
    public ResponseEntity<?> rimuoviTappaItinerario(@RequestParam String usernameContributor, @RequestParam Integer
            idItinerario, @RequestParam Integer idPunto) {
        try {
            itinerarioService.rimuoviTappa(usernameContributor, idItinerario, idPunto);
            return ResponseEntity.ok("L'utente '" + usernameContributor + "' ha eliminato il punto di interesse '" + idPunto + "' dall'itinerario '" + idItinerario + "'.");
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }


}
