package ids.unicam.controller;

import ids.unicam.Service.ComuneService;
import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.models.Comune;
import ids.unicam.models.DTO.RichiestaCreazioneComuneDTO;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import ids.unicam.models.attori.Ruolo;
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

    @PutMapping("/cambioRuolo/{usernameContributor}/{ruolo}")
    public void cambiaRuolo(@PathVariable String usernameContributor,@PathVariable  Ruolo ruolo){
        gestorePiattaformaService.cambiaRuolo(usernameContributor,ruolo);
    }
}
