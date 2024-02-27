package ids.unicam.controller;

import ids.unicam.Service.CuratoreService;
import ids.unicam.Service.PoiService;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.Punto;
import ids.unicam.models.contenuti.puntiInteresse.DayOfWeek;
import ids.unicam.models.contenuti.puntiInteresse.Orario;
import ids.unicam.models.contenuti.puntiInteresse.Tag;
import ids.unicam.models.contenuti.puntiInteresse.TipologiaPuntoInteresse;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.annotations.Range;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;
import java.time.LocalTime;

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
    public ResponseEntity<?> setOrario(Integer idPunto, DayOfWeek day, @Range(from = 0, to = 24) int oraApertura, @Range(from = 0, to = 60) int minutiApertura, @Range(from = 0, to = 24) int oraChiusura, @Range(from = 0, to = 60) int minutiChiusura) {
        poiService.setOrario(idPunto, day, new Orario.OrarioApertura(LocalTime.of(oraApertura, minutiApertura), LocalTime.of(oraChiusura, minutiChiusura)));
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
            return ResponseEntity.ok("Il contributor con id '"+usernameContributor+"' ha aggionrato la scadenza a '"+scadenza+ "' del punto di interesse con id '"+idPuntoInteresse+"' .");
        } catch (UnsupportedOperationException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }

    }

    @PutMapping("/condividi/{idPunto}")
    public ResponseEntity<?> condividi(@RequestParam String usernameCuratore, @PathVariable Integer idPunto) {
        try {
            curatoreService.condividi(usernameCuratore, idPunto);
            return ResponseEntity.ok("L'utente con id '"+usernameCuratore+"' ha condiviso il punto di interesse con id '"+usernameCuratore+"' .");
        } catch (IllegalArgumentException | UnsupportedOperationException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @GetMapping("/materiali/{idPunto}")
    public ResponseEntity<?> getMateriali(@PathVariable Integer idPunto) {
        return ResponseEntity.ok(poiService.getMaterialiPoi(idPunto));
    }
}
