package ids.unicam.controller;

import ids.unicam.Service.ComuneService;
import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.Comune;
import ids.unicam.models.DTO.RichiestaCreazioneComuneDTO;
import ids.unicam.models.attori.Ruolo;
import ids.unicam.models.attori.TuristaAutenticato;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;




@RestController
@RequestMapping("/comune")
public class  ComuneController implements ControllerBase<RichiestaCreazioneComuneDTO, Integer>{

    private final ComuneService comuneService;
    private final GestorePiattaformaService gestorePiattaformaService;

    public ComuneController(ComuneService comuneService, GestorePiattaformaService gestorePiattaformaService) {
        this.comuneService = comuneService;
        this.gestorePiattaformaService = gestorePiattaformaService;
    }

    @Override
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(comuneService.findAll());
    }

    @Override
    public ResponseEntity<?> getById(Integer id) {
        return ResponseEntity.ok(comuneService.findById(id));
    }

    @GetMapping("/{nomeComune}")
    public ResponseEntity<?> getByName(@PathVariable String nomeComune) {
        return ResponseEntity.ok(comuneService.getComuneByNome(nomeComune));
    }

    @Override

    public ResponseEntity<?> create(RichiestaCreazioneComuneDTO comuneDTO) {
        try {
            return ResponseEntity.ok(comuneService.creaComune(new Comune(comuneDTO)));
        } catch (ConnessioneFallitaException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public ResponseEntity<?> update(RichiestaCreazioneComuneDTO comuneDTO, Integer id) {
        try {
            Comune comune = comuneService.update(new Comune(comuneDTO), id);
            return new ResponseEntity<>(comune, HttpStatus.OK);
        } catch (ConnessioneFallitaException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.OK);
        }
    }

    @Override
    public ResponseEntity<?> delete(Integer id) {
        comuneService.deleteById(id);
        return ResponseEntity.ok("{}");
    }

    @DeleteMapping("/{nomeComune}")
    public ResponseEntity<?> delete(@PathVariable String nomeComune) {
        comuneService.deleteByNome(nomeComune);
        return ResponseEntity.ok("{}");
    }

    @PutMapping("/cambioRuolo/{username}/{ruolo}")
    public ResponseEntity<?> cambiaRuolo(@PathVariable String username,@PathVariable  Ruolo ruolo){
        try {
            TuristaAutenticato nuovo = gestorePiattaformaService.cambiaRuolo(username, ruolo);
            return new ResponseEntity<>(nuovo,HttpStatus.OK);
        } catch (ConnessioneFallitaException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }
}
