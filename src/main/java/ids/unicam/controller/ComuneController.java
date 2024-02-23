package ids.unicam.controller;

import ids.unicam.Service.ComuneService;
import ids.unicam.models.Comune;
import ids.unicam.models.DTO.RichiestaCreazioneComuneDTO;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;


@RestController
@RequestMapping("/comune")
public class  ComuneController implements ControllerBase<RichiestaCreazioneComuneDTO, Integer>{

    private final ComuneService comuneService;

    public ComuneController(ComuneService comuneService) {
        this.comuneService = comuneService;
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
        return ResponseEntity.ok(comuneService.creaComune(new Comune(comuneDTO)));
    }

    @Override
    public ResponseEntity<?> update(RichiestaCreazioneComuneDTO comuneDTO, Integer id) {
        return ResponseEntity.ok(comuneService.update(new Comune(comuneDTO), id));
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
}
