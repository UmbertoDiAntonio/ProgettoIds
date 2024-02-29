package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.ComuneRepository;
import ids.unicam.Service.ComuneService;
import ids.unicam.models.Comune;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Service
public class ComuneServiceImpl implements ComuneService {
    private final ComuneRepository repository;
    private final AnimatoreServiceImpl animatoreService;
    private final ContributorServiceImpl contributorService;
    private final ContributorAutorizzatoServiceImpl contributorAutorizzatoService;
    private final CuratoreServiceImpl curatoreService;
    private final PoiServiceImpl poiService;

    @Autowired
    public ComuneServiceImpl(ComuneRepository repository, AnimatoreServiceImpl animatoreService,
                             ContributorServiceImpl contributorService,
                             ContributorAutorizzatoServiceImpl contributorAutorizzatoService,
                             CuratoreServiceImpl curatoreService, PoiServiceImpl poiService) {
        this.repository = repository;
        this.animatoreService = animatoreService;
        this.contributorService = contributorService;
        this.contributorAutorizzatoService = contributorAutorizzatoService;
        this.curatoreService = curatoreService;
        this.poiService = poiService;
    }


    @Override
    public void deleteByNome(String nomeComune){
            repository.deleteById(nomeComune);
        }



    public Comune save(Comune comune) {
        return repository.save(comune);
    }


    public Comune creaComune(Comune comune) {
        return save(comune);
    }

    @Override
    public Optional<Comune> findById(String nomeComune) {
        return repository.findById(nomeComune);
    }

    @Override
    public List<Comune> findAll() {
        return repository.findAll();
    }

    @Override
    public List<Animatore> getAnimatoriDelComune(String nome_comune) {
        return animatoreService.findByNomeComune(nome_comune);
    }

    @Override
    public List<Contributor> getContributorDelComune(String nome_comune) {
        return contributorService.findByNomeComune(nome_comune);
    }

    @Override
    public List<ContributorAutorizzato> getContributorAutorizzatiDelComune(String nome_comune) {
        return contributorAutorizzatoService.findByNomeComune(nome_comune);
    }

    @Override
    public List<Curatore> getCuratoriDelComune(String nome_comune) {
        return curatoreService.findByNomeComune(nome_comune);
    }


    @Override
    public Optional<Comune> getComuneByNome(String nomeComune) throws IllegalArgumentException {
        return repository.findComuneByNomeIgnoreCase(nomeComune);
    }

    @Override
    public List<PuntoInteresse> getPuntiInteresseNelComune(String nomeComune) throws IllegalArgumentException {
        Optional<Comune> oComune = getComuneByNome(nomeComune);
        if (oComune.isPresent()) {
            Comune comune = oComune.get();
            List<PuntoInteresse> puntiInteresseNelComune = new ArrayList<>();
            List<PuntoInteresse> tuttiPuntiInteresse = poiService.findActive();
            for (PuntoInteresse puntoInteresse : tuttiPuntiInteresse) {
                if (comune.verificaCoordinateComune(puntoInteresse.getPt())) {
                    puntiInteresseNelComune.add(puntoInteresse);
                }
            }
            return puntiInteresseNelComune;
        } else {
            return Collections.emptyList();
        }
    }

}
