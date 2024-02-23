package ids.unicam.controller;

import ids.unicam.Service.CuratoreService;
import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/Curatore")
public class CuratoreController implements ControllerBase<RichiestaCreazioneContributorDTO, String> {

    private final CuratoreService curatoreService;
    private final GestorePiattaformaService gestorePiattaformaService;

    public CuratoreController(CuratoreService curatoreService, GestorePiattaformaService gestorePiattaformaService) {
        this.curatoreService = curatoreService;
        this.gestorePiattaformaService = gestorePiattaformaService;
    }

    @Override
    public ResponseEntity<?> getAll() {
        return ResponseEntity.ok(curatoreService.getAll());
    }

    @Override
    public ResponseEntity<?> getById(String username) {
        return ResponseEntity.ok(curatoreService.getById(username));
    }

    @Override
    public ResponseEntity<?> create(RichiestaCreazioneContributorDTO contributorDTO) {
        try {
            return new ResponseEntity<>(gestorePiattaformaService.registraContributor(contributorDTO),HttpStatus.OK);
        } catch (ConnessioneFallitaException e) {
            return new ResponseEntity<>(e.getMessage(), HttpStatus.BAD_REQUEST);
        }
    }

    @Override
    public ResponseEntity<?> delete(String username) {
        curatoreService.deleteById(username);
        return ResponseEntity.ok("{}");
    }

    @DeleteMapping("eliminaItinerario")
    public void eliminaItinerario(@RequestParam String usernameCuratore,@RequestParam Integer idItinerario){
        curatoreService.eliminaItinerario(usernameCuratore, idItinerario);
    }
    @DeleteMapping("eliminaPuntoIntesse")
    public void eliminaPuntoInteresse(@RequestParam String usernameCuratore,@RequestParam Integer idPuntoInteresse){
        curatoreService.eliminaItinerario(usernameCuratore, idPuntoInteresse);
    }
    @DeleteMapping("eliminaContest")
    public void eliminaContest(@RequestParam String usernameCuratore, @RequestParam Integer idContest){
        curatoreService.eliminaItinerario(usernameCuratore, idContest);
    }
    @PutMapping("valutaPuntoInteresse")
    public void valutaPuntoInteresse(@RequestParam String usernameCuratore,@RequestParam Integer idPunto,@RequestParam Boolean stato){
        curatoreService.valutaPuntoInteresse(usernameCuratore, idPunto,stato);
    }
    @PutMapping("valutaMateriale")
    public void valutaMateriale(@RequestParam String usernameCuratore,@RequestParam Integer idMateriale,@RequestParam Boolean stato){
        curatoreService.valutaMateriale(usernameCuratore, idMateriale,stato);
    }

    @PutMapping("segui/{usernameCuratore}")
    public void subscribeOsservatore(@PathVariable String usernameCuratore,@RequestParam String usernameContributor){
        curatoreService.aggiungiOsservatore(usernameCuratore,usernameContributor);
    }
    @PutMapping("unsuscribe/{usernameCuratore}")
    public void unsubscibeOsservatore(@PathVariable String usernameCuratore,@RequestParam String usernameContributor){
        curatoreService.rimuoviOsservatore(usernameCuratore,usernameContributor);
    }
    @PutMapping("getNotifiche/{usernameCuratore}")
    public void getNotifiche(@PathVariable String usernameCuratore){
        curatoreService.getNotifiche(usernameCuratore);
    }

}
