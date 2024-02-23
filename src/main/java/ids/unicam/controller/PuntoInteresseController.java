package ids.unicam.controller;

import ids.unicam.Service.CuratoreService;
import ids.unicam.Service.PoiService;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.DTO.PuntoInteresseDTO;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;

@RestController
@RequestMapping("/PuntoInteresse")
public class PuntoInteresseController implements ControllerBase<PuntoInteresseDTO, Integer> {
    private final PoiService poiService;
    private final CuratoreService curatoreService;

    public PuntoInteresseController(PoiService poiService, CuratoreService curatoreService) {
        this.poiService = poiService;
        this.curatoreService = curatoreService;
    }

    @Override
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(poiService.findActive());
    }

    @Override
    public ResponseEntity<?> getById(Integer id) {
        return ResponseEntity.ok(poiService.getById(id));
    }

    @Override
    public ResponseEntity<?> create(PuntoInteresseDTO poiDTO) {
        try {
            return ResponseEntity.ok(poiService.creaPuntoInteresse(new PuntoInteresse(poiDTO)));
        } catch (FuoriComuneException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @Override
    public ResponseEntity<?> delete(Integer integer) {
        return null;
    }

    @PutMapping("/setScadenza")
    public ResponseEntity<?> modificaScadenza(@RequestParam String usernameContributor, @RequestParam Integer idPuntoInteresse,@RequestParam LocalDate scadenza){
        try {
            poiService.modificaScadenza(usernameContributor, idPuntoInteresse, scadenza);
            return ResponseEntity.ok("{}");
        }catch (UnsupportedOperationException e){
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }

    }

    @PutMapping("/condividi/{idPunto}")
    public void condividi(@RequestParam String usernameCuratore,@PathVariable Integer idPunto){
        curatoreService.condividi(usernameCuratore,idPunto);
    }

    @GetMapping("/materiali/{idPunto}")
    public void getMateriali(@PathVariable Integer idPunto){
        poiService.getMaterialiPoi(idPunto);
    }
}
