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
public class  ComuneController implements ControllerBase<RichiestaCreazioneComuneDTO, String>{

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
    @GetMapping("/{nomeComune}")
    public ResponseEntity<?> getById(@PathVariable String nomeComune) {
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


    @DeleteMapping("/{nomeComune}")
    public ResponseEntity<?> delete(@PathVariable String nomeComune) {
        comuneService.deleteById(nomeComune);
        return ResponseEntity.ok("{}");
    }

    @PutMapping("/cambioRuolo/{username}/{ruolo}")
    public ResponseEntity<?> cambiaRuolo(@PathVariable String username,@PathVariable  Ruolo ruolo){
        try {
            TuristaAutenticato nuovo = gestorePiattaformaService.cambiaRuolo(username, ruolo);
            return new ResponseEntity<>(nuovo,HttpStatus.OK);
        } catch (ConnessioneFallitaException  | IllegalArgumentException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }
}
