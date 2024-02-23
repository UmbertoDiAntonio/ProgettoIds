package ids.unicam.controller;

import ids.unicam.Service.ItinerarioService;
import ids.unicam.models.DTO.RichiestaCreazioneItinerarioDTO;
import ids.unicam.models.contenuti.Itinerario;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/Itinerario")
public class ItinerarioController implements ControllerBase<RichiestaCreazioneItinerarioDTO, Integer> {

    private final ItinerarioService itinerarioService;

    public ItinerarioController(ItinerarioService itinerarioService) {
        this.itinerarioService = itinerarioService;
    }

    @Override
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(itinerarioService.getAll());
    }

    @Override
    public ResponseEntity<?> getById(Integer id) {
        return ResponseEntity.ok(itinerarioService.getById(id));
    }

    @Override
    public ResponseEntity<?> create(RichiestaCreazioneItinerarioDTO itinerarioDTO) {

            return ResponseEntity.ok(itinerarioService.creaItinerario(new Itinerario(itinerarioDTO)));


    }

    @Override
    public ResponseEntity<?> delete(Integer id) {
        itinerarioService.deleteById(id);
        return ResponseEntity.ok("{}");
    }

    @PutMapping("/aggiungiTappa")
    public ResponseEntity<?> aggiungiTappaItinerario(@RequestParam String usernameContributor, @RequestParam Integer idItinerario, @RequestParam Integer idTappa) {
        try {
            return ResponseEntity.ok(itinerarioService.aggiungiTappa(usernameContributor, idItinerario, idTappa));
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);

        }
    }

    @PutMapping("/rimuoviTappa")
    public ResponseEntity<?> rimuoviTappaItinerario(@RequestParam String usernameContributor, @RequestParam Integer
            idItinerario, @RequestParam Integer idPunto) {
        try {
            itinerarioService.rimuoviTappa(usernameContributor, idItinerario, idPunto);
            return ResponseEntity.ok("{}");
        } catch (IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }


}
