package ids.unicam.controller;

import ids.unicam.Service.AnimatoreService;
import ids.unicam.Service.ContestService;
import ids.unicam.Service.MaterialeService;
import ids.unicam.Service.PoiService;
import ids.unicam.exception.ContestException;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.materiali.TipologiaMateriale;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

@RestController
@RequestMapping("/materiali")
public class MaterialeController{
    private final MaterialeService materialeService;
    private final PoiService poiService;
    private final ContestService contestService;

    public MaterialeController(MaterialeService materialeService, PoiService poiService, ContestService contestService) {
        this.materialeService = materialeService;
        this.poiService = poiService;
        this.contestService = contestService;
    }

    @GetMapping("/getAll")
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(materialeService.getAll());
    }

    @GetMapping("/{id}")
    public ResponseEntity<?> getById(@PathVariable Integer id) {
        return ResponseEntity.ok(materialeService.getById(id));
    }

    @PostMapping(value = "/caricaMateriale",  consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResponseEntity<?> fileUpload( @RequestParam("materiale") MultipartFile materiale, @RequestParam String usernameTurista,@RequestParam  Integer idContenitore,@RequestParam TipologiaMateriale tipologia) throws IOException {
        File newFile = new File("src/main/resources/materials/" + materiale.getOriginalFilename());
        if (!newFile.createNewFile())
            return new ResponseEntity<>("Materiale già caricato", HttpStatus.BAD_REQUEST);
        FileOutputStream fileOutputStream = new FileOutputStream(newFile);
        fileOutputStream.write(materiale.getBytes());
        fileOutputStream.close();
        try {
            MaterialeGenerico materialeGenerico=materialeService.crea(materiale.getOriginalFilename(),tipologia,usernameTurista);

            if(poiService.getById(idContenitore).isEmpty())
                contestService.aggiungiMateriale(usernameTurista,idContenitore,materialeGenerico);
            else
                poiService.aggiungiMateriale(usernameTurista,idContenitore,materialeGenerico);

        } catch (FuoriComuneException | IllegalArgumentException | ContestException e) {
            throw new RuntimeException(e);
        }
        return new ResponseEntity<>("Materiale Caricato",HttpStatus.OK);
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<?> delete(@PathVariable Integer id) {
        materialeService.deleteById(id);
        return ResponseEntity.ok("Il materiale con id '"+id+"' e' stato eliminato.");
    }

    @GetMapping("/getBase64/{id}")
    public ResponseEntity<?> getBase64(@PathVariable Integer id) {
        return ResponseEntity.ok(materialeService.getBase64ById(id));
    }

}
