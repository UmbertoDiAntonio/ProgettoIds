package ids.unicam.controller;

import ids.unicam.Service.ContestService;
import ids.unicam.Service.MaterialeService;
import ids.unicam.Service.PoiService;
import ids.unicam.Service.TuristaAutenticatoService;
import ids.unicam.exception.ContestException;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.materiali.TipologiaMateriale;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Optional;

@RestController
@RequestMapping("/materiali")
public class MaterialeController {
    private final MaterialeService materialeService;
    private final PoiService poiService;
    private final ContestService contestService;
    private final TuristaAutenticatoService turistaAutenticatoService;

    public MaterialeController(MaterialeService materialeService, PoiService poiService, ContestService contestService, TuristaAutenticatoService turistaAutenticatoService) {
        this.materialeService = materialeService;
        this.poiService = poiService;
        this.contestService = contestService;
        this.turistaAutenticatoService = turistaAutenticatoService;
    }

    @GetMapping("/getAll")
    @Operation(summary = "Elenco dei materiali",
            description = "Un elenco dei materiali che sono salvati nel database.")
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(materialeService.getAll());
    }

    @GetMapping("/{id}")
    @Operation(summary = "Materiale dall'identificatore univoco id",
            description = "Materiale dall'identificatore univoco id salvato nel database.")
    public ResponseEntity<?> getById(
            @Parameter(description = "id del materiale") @PathVariable Integer id) {
        return ResponseEntity.ok(materialeService.getById(id));
    }

    @PostMapping(value = "/caricaMateriale", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    @Operation(summary = "Carica un materiale",
            description = "Caricamento di un materiale.")
    public ResponseEntity<?> fileUpload(
            @Parameter(description = "scelta del file") @RequestParam("materiale") MultipartFile materiale,
            @Parameter(description = "username utente") @RequestParam String usernameTurista,
            @Parameter(description = "id dell'oggetto che dovrà contenere il materiale") @RequestParam Integer idContenitore,
            @Parameter(description = "scelta del tipo di materiale da caricare") @RequestParam TipologiaMateriale tipologia) throws IOException {
        File newFile = new File("src/main/resources/materials/" + materiale.getOriginalFilename());
        if (!newFile.createNewFile())
            return new ResponseEntity<>("Materiale già caricato", HttpStatus.BAD_REQUEST);
        FileOutputStream fileOutputStream = new FileOutputStream(newFile);
        fileOutputStream.write(materiale.getBytes());
        fileOutputStream.close();
        try {
            Optional<TuristaAutenticato> oTurista = turistaAutenticatoService.getByUsername(usernameTurista);
            if (oTurista.isEmpty()) {
                return new ResponseEntity<>("Username non valido", HttpStatus.BAD_REQUEST);
            }
            TuristaAutenticato turistaAutenticato = oTurista.get();
            MaterialeGenerico materialeGenerico = materialeService.crea(materiale.getOriginalFilename(), tipologia, turistaAutenticato);

            turistaAutenticatoService.aggiungiMateriale(usernameTurista, idContenitore, materialeGenerico);

            return new ResponseEntity<>("Il Materiale con id '" + materialeGenerico.getId() + "' e' stato caricato dall'utente con username '" + usernameTurista + "'", HttpStatus.OK);
        } catch (IllegalStateException | IllegalArgumentException | ContestException | FuoriComuneException e) {
            throw new RuntimeException(e);
        }
    }

    @DeleteMapping("/{id}")
    @Operation(summary = "Elimina materiale",
            description = "Eliminazione di un materiale dall'identificatore univoco id.")
    public ResponseEntity<?> delete(
            @Parameter(description = "id del materiale da eliminare") @PathVariable Integer id) {
        materialeService.deleteById(id);
        return ResponseEntity.ok("Il materiale con id '" + id + "' e' stato eliminato.");
    }

    @GetMapping("/getBase64/{id}")
    @Operation(summary = "Ottieni codifica del materiale in base64",
            description = "Ottenere la codifica del materiale caricato in base64.")
    public ResponseEntity<?> getBase64(
            @Parameter(description = "id del materiale") @PathVariable Integer id) {
        return ResponseEntity.ok(materialeService.getBase64ById(id));
    }

}
