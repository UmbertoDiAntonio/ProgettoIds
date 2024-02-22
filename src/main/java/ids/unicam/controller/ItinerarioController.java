package ids.unicam.controller;

import ids.unicam.Service.ContributorService;
import ids.unicam.Service.ItinerarioService;
import ids.unicam.models.DTO.RichiestaCreazioneItinerarioDTO;
import ids.unicam.models.contenuti.Itinerario;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
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
    public ResponseEntity<?> update(RichiestaCreazioneItinerarioDTO itinerarioDTO, Integer id) {
        return ResponseEntity.ok(itinerarioService.update(new Itinerario(itinerarioDTO), id));
    }

    @Override
    public ResponseEntity<?> delete(Integer id) {
        itinerarioService.deleteById(id);
        return ResponseEntity.ok("{}");
    }

    @PutMapping("/aggiungiTappa")
    public boolean aggiungiTappaItinerario(String usernameContributor,Integer idItinerario,Integer idTappa){
        return  itinerarioService.aggiungiTappa(usernameContributor,idItinerario,idTappa);
    }
}