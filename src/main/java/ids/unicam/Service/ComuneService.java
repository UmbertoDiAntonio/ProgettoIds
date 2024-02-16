package ids.unicam.Service;

import ids.unicam.DataBase.Repository.ComuneRepository;
import ids.unicam.models.Comune;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import ids.unicam.models.users.organizzazioneComune.Animatore;
import ids.unicam.models.users.organizzazioneComune.Contributor;
import ids.unicam.models.users.organizzazioneComune.ContributorAutorizzato;
import ids.unicam.models.users.organizzazioneComune.Curatore;
import org.jetbrains.annotations.Nullable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Service
public class ComuneService {
    private final ComuneRepository comuneRepository;
    private final AnimatoreService animatoreService;
    private final ContributorService contributorService;
    private final ContributorAutorizzatoService contributorAutorizzatoService;
    private final CuratoreService curatoreService;
    private final PoiService poiService;

    @Autowired
    public ComuneService(ComuneRepository comuneRepository, AnimatoreService animatoreService,
                         ContributorService contributorService,
                         ContributorAutorizzatoService contributorAutorizzatoService,
                         CuratoreService curatoreService, PoiService poiService) {
        this.comuneRepository = comuneRepository;
        this.animatoreService = animatoreService;
        this.contributorService = contributorService;
        this.contributorAutorizzatoService = contributorAutorizzatoService;
        this.curatoreService = curatoreService;
        this.poiService = poiService;
    }


    public void deleteById(int id) {
        comuneRepository.deleteById(id);
    }


    public Comune save(Comune comune) {
        comune = comuneRepository.save(comune);
        return comune;
    }

    public Comune creaComune(String nomeComune){
        return save(new Comune(nomeComune));
    }

    public Optional<Comune> findById(int id) {
        return comuneRepository.findById(id);
    }


    public List<Comune> findAll() {
        return comuneRepository.findAll();
    }

    public void deleteAll() {
        comuneRepository.deleteAll();
    }

    public List<Animatore> getAnimatoriByComune(String nome_comune){
        return animatoreService.findByNomeComune(nome_comune);
    }
    public List<Contributor> getContributorByComune(String nome_comune){
        return contributorService.findByNomeComune(nome_comune);
    }
    public List<ContributorAutorizzato> getContributorAutorizzatiByComune(String nome_comune){
        return contributorAutorizzatoService.findByNomeComune(nome_comune);
    }
    public List<Curatore> getCuratoriByComune(String nome_comune){
        return curatoreService.findByNomeComune(nome_comune);
    }

    public void eliminaContributor(int id) {
        contributorService.deleteById(id);
    }

    public @Nullable Comune findByNome(String nomeComune) {
        Optional<Comune> oComune = comuneRepository.findByNome(nomeComune);
        return oComune.orElse(null);
    }

    public List<PuntoInteresse> getPuntiInteresseNelComune(String nomeComune){
        Comune comune = findByNome(nomeComune);
        if (comune != null) {
            List<PuntoInteresse> puntiInteresseNelComune = new ArrayList<>();
            List<PuntoInteresse> tuttiPuntiInteresse = poiService.findAll();
            for (PuntoInteresse puntoInteresse : tuttiPuntiInteresse) {
                if (comune.verificaCoordinateComune(puntoInteresse.getPt())) {
                    puntiInteresseNelComune.add(puntoInteresse);
                }
            }
            return puntiInteresseNelComune;
        } else {
            return Collections.emptyList(); // Nessun comune trovato con quel nome
        }
    }
}
