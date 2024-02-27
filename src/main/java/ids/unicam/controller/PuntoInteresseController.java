package ids.unicam.controller;

import ids.unicam.Service.CuratoreService;
import ids.unicam.Service.PoiService;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.Punto;
import ids.unicam.models.contenuti.puntiInteresse.Orario;
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

    @GetMapping("/getAsList")
    public ResponseEntity<?> getAsList() {
        return ResponseEntity.ok(poiService.getAsList());
    }

    @GetMapping("/getAsListDetailed")
    public ResponseEntity<?> getAsListDetailed() {
        return ResponseEntity.ok(poiService.getAsListDetailed());
    }

    @PutMapping("/setOrario")
    public ResponseEntity<?> setOrario(Integer idPunto, Orario orario) {
        poiService.setOrario(idPunto,orario);

        return ResponseEntity.ok("");
    }


    @GetMapping("/{id}")
    public ResponseEntity<?> getById(Integer id) {
        return ResponseEntity.ok(poiService.getById(id));
    }

    @PostMapping
    public ResponseEntity<?> create(@RequestParam String nomePOI, @RequestParam double latitudine, @RequestParam double longitudine, @RequestParam String usernameCreatore, @RequestParam @Nullable String tag, @RequestParam TipologiaPuntoInteresse tipologiaPuntoInteresse) {
        try {
            return ResponseEntity.ok(poiService.creaPuntoInteresse(nomePOI, new Punto(latitudine, longitudine), usernameCreatore, new Tag(tag), tipologiaPuntoInteresse));
        } catch (FuoriComuneException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @PutMapping("/addTag")
    public ResponseEntity<?> aggiungiTag(@RequestParam String nomeTag, @RequestParam Integer idPuntoInteresse) {
        try {
            poiService.aggiungiTag(idPuntoInteresse, new Tag(nomeTag));
            return ResponseEntity.ok("Aggiunto tag '" + nomeTag + "' al punto di interesse: '" + idPuntoInteresse + "' .");
        } catch (UnsupportedOperationException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }

    }

    @DeleteMapping("/{id}")
    public ResponseEntity<?> delete(Integer id) {
        poiService.deleteById(id);
        return ResponseEntity.ok("Punto Interesse: '" + id + "' eliminato");
    }

    @PutMapping("/setScadenza")
    public ResponseEntity<?> modificaScadenza(@RequestParam String usernameContributor, @RequestParam Integer idPuntoInteresse, @RequestParam LocalDate scadenza) {
        try {
            poiService.modificaScadenza(usernameContributor, idPuntoInteresse, scadenza);
            return ResponseEntity.ok("{}");
        } catch (UnsupportedOperationException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }

    }

    @PutMapping("/condividi/{idPunto}")
    public ResponseEntity<?> condividi(@RequestParam String usernameCuratore, @PathVariable Integer idPunto) {
        try {
            curatoreService.condividi(usernameCuratore, idPunto);
            return ResponseEntity.ok("{}");
        } catch (IllegalArgumentException | UnsupportedOperationException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @GetMapping("/materiali/{idPunto}")
    public ResponseEntity<?> getMateriali(@PathVariable Integer idPunto) {
        return ResponseEntity.ok(poiService.getMaterialiPoi(idPunto));
    }
}
