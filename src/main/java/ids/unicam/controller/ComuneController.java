package ids.unicam.controller;

import ids.unicam.Service.ComuneService;
import ids.unicam.models.Comune;
import ids.unicam.models.DTO.RichiestaCreazioneComuneDTO;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


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
}
