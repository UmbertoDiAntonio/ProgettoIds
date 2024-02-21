package ids.unicam.controller;

import ids.unicam.Service.PoiService;
import ids.unicam.models.DTO.RichiestaCreazionePoiDTO;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/PuntoInteresse")
public class PuntoInteresseController implements ControllerBase<RichiestaCreazionePoiDTO, Integer> {
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
    public ResponseEntity<?> create(RichiestaCreazionePoiDTO poiDTO) {
        return ResponseEntity.ok(poiService.creaPuntoInteresse(new PuntoInteresse(poiDTO)));
    }

    @Override
    public ResponseEntity<?> update(RichiestaCreazionePoiDTO entity, Integer integer) {
        return null;
    }

    @Override
    public ResponseEntity<?> delete(Integer integer) {
        return null;
    }
}
