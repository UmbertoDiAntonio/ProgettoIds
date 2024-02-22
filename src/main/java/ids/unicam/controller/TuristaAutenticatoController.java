package ids.unicam.controller;

import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.Service.TuristaAutenticatoService;
import ids.unicam.models.DTO.RichiestaCreazioneTuristaDTO;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/TuristaAutenticato")
public class TuristaAutenticatoController implements ControllerBase<RichiestaCreazioneTuristaDTO, String> {

    private final TuristaAutenticatoService turistaAutenticatoService;
    private final GestorePiattaformaService gestorePiattaformaService;

    public TuristaAutenticatoController(TuristaAutenticatoService turistaAutenticatoService, GestorePiattaformaService gestorePiattaformaService) {
        this.turistaAutenticatoService = turistaAutenticatoService;
        this.gestorePiattaformaService = gestorePiattaformaService;
    }

    @Override
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(turistaAutenticatoService.getAll());
    }

    @Override
    public ResponseEntity<?> getById(String username) {
        return ResponseEntity.ok(turistaAutenticatoService.getById(username));
    }

    @Override
    public ResponseEntity<?> create(RichiestaCreazioneTuristaDTO turistaDTO) {
        return ResponseEntity.ok(gestorePiattaformaService.registraTurista(turistaDTO));
    }

    @Override
    public ResponseEntity<?> update(RichiestaCreazioneTuristaDTO turistaDTO, String username) {
        return ResponseEntity.ok(turistaAutenticatoService.update(turistaDTO,username));
    }

    @Override
    public ResponseEntity<?> delete(String username) {
        turistaAutenticatoService.deleteById(username);
        return ResponseEntity.ok("{}");
    }
}
