package ids.unicam.models.Service;

import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.contenuti.PuntoInteresse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.List;

@Service
public class GestioneComuneService {

    private final ComuneService comuneService;

    @Autowired
    public GestioneComuneService(ComuneService comuneService) {
        this.comuneService = comuneService;
    }

    public void eliminaContributor(int id){
        comuneService.eliminaContributor(id);
    }
    public List<Animatore> getAnimatoriDelComune(String nomeComune) {
        return comuneService.getAnimatoriByComune(nomeComune);
    }

    public List<Contributor> getContributorDelComune(String nomeComune) {
        return comuneService.getContributorByComune(nomeComune);
    }

    public List<ContributorAutorizzato> getContributorAutorizzatiDelComune(String nomeComune) {
        return comuneService.getContributorAutorizzatiByComune(nomeComune);
    }

    public List<Curatore> getCuratoriDelComune(String nomeComune) {
        return comuneService.getCuratoriByComune(nomeComune);
    }

    public List<PuntoInteresse> getContenuti(String nomeComune) {
        return comuneService.getPuntiInteresseNelComune(nomeComune);
    }
}
