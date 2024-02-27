package ids.unicam.controller;

import ids.unicam.Service.CuratoreService;
import ids.unicam.Service.PoiService;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.Punto;
import ids.unicam.models.contenuti.puntiInteresse.Tag;
import ids.unicam.models.contenuti.puntiInteresse.TipologiaPuntoInteresse;
import org.jetbrains.annotations.Nullable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;

@RestController
@RequestMapping("/PuntoInteresse")
public class PuntoInteresseController {
    private final PoiService poiService;
    private final CuratoreService curatoreService;

    public PuntoInteresseController(PoiService poiService, CuratoreService curatoreService) {
        this.poiService = poiService;
        this.curatoreService = curatoreService;
    }

    @GetMapping("/getAll")
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(poiService.findActive());
    }

    @GetMapping("/{id}")
    public ResponseEntity<?> getById(Integer id) {
        return ResponseEntity.ok(poiService.getById(id));
    }

    @PostMapping
    public ResponseEntity<?> create(@RequestParam String nomePOI, @RequestParam double latitudine, @RequestParam double longitudine, @RequestParam String usernameCreatore, @RequestParam @Nullable String tag, @RequestParam TipologiaPuntoInteresse tipologiaPuntoInteresse) {
        try {
            return ResponseEntity.ok(poiService.creaPuntoInteresse(nomePOI, new Punto(latitudine,longitudine), usernameCreatore, new Tag(tag), tipologiaPuntoInteresse));
        } catch (FuoriComuneException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @PutMapping("/addTag")
    public ResponseEntity<?> aggiungiTag(@RequestParam String nomeTag, @RequestParam Integer idPuntoInteresse){
        try {
            poiService.aggiungiTag(idPuntoInteresse, new Tag(nomeTag));
            return ResponseEntity.ok("{}");
        }catch (UnsupportedOperationException e){
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }

    }

    @DeleteMapping("/{id}")
    public ResponseEntity<?> delete(Integer id) {
        poiService.deleteById(id);
        return ResponseEntity.ok("Punto Interesse: '" + id + "' eliminato");
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
