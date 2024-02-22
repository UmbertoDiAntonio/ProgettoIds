package ids.unicam.controller;

import ids.unicam.Service.ContributorAutorizzatoService;
import ids.unicam.Service.CuratoreService;
import ids.unicam.Service.GestorePiattaformaService;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

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
        return ResponseEntity.ok(gestorePiattaformaService.registraContributor(contributorDTO));
    }

    @Override
    public ResponseEntity<?> update(RichiestaCreazioneContributorDTO contributorDTO, String username) {
        return ResponseEntity.ok(curatoreService.update(contributorDTO, username));
    }

    @Override
    public ResponseEntity<?> delete(String username) {
        curatoreService.deleteById(username);
        return ResponseEntity.ok("{}");
    }

    public void eliminaItinerario(String usernameCuratore, Integer idItinerario){
        curatoreService.eliminaItinerario(usernameCuratore, idItinerario);
    }

    public void eliminaPuntoInteresse(String usernameCuratore, Integer idPuntoInteresse){
        curatoreService.eliminaItinerario(usernameCuratore, idPuntoInteresse);
    }

    public void eliminaContest(String usernameCuratore, Integer idContest){
        curatoreService.eliminaItinerario(usernameCuratore, idContest);
    }





    public void valutaPuntoInteresse(String usernameCuratore,Integer idPunto,Boolean stato){
        curatoreService.valutaPuntoInteresse(usernameCuratore, idPunto,stato);
    }
    public void valutaMateriale(String usernameCuratore,Integer idMateriale,Boolean stato){
        curatoreService.valutaMateriale(usernameCuratore, idMateriale,stato);
    }

}
