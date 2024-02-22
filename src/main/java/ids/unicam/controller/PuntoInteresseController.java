package ids.unicam.controller;

import ids.unicam.Service.PoiService;
import ids.unicam.models.DTO.PuntoInteresseDTO;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.time.LocalDate;

@RestController
@RequestMapping("/PuntoInteresse")
public class PuntoInteresseController implements ControllerBase<PuntoInteresseDTO, Integer> {
    private final PoiService poiService;

    public PuntoInteresseController(PoiService poiService) {
        this.poiService = poiService;
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
        return ResponseEntity.ok(poiService.creaPuntoInteresse(new PuntoInteresse(poiDTO)));
    }

    @Override
    public ResponseEntity<?> update(PuntoInteresseDTO entity, Integer integer) {
        return null;
    }

    @Override
    public ResponseEntity<?> delete(Integer integer) {
        return null;
    }

    @PutMapping("setScadenza")
    public void modificaScadenza(@RequestParam String usernameContributor, @RequestParam Integer idPuntoInteresse,@RequestParam LocalDate scadenza){
        poiService.modificaScadenza(usernameContributor,idPuntoInteresse,scadenza);
    }
}
