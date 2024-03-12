package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.ComuneRepository;
import ids.unicam.Service.*;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.Comune;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

@Service
public class ComuneServiceImpl implements ComuneService {
    private final ComuneRepository repository;
    private final AnimatoreService animatoreService;
    private final ContributorService contributorService;
    private final ContributorAutorizzatoService contributorAutorizzatoService;
    private final CuratoreService curatoreService;
    private final PoiService poiService;
    private final ItinerarioService itinerarioService;
    private final ContestService contestService;

    @Autowired
    public ComuneServiceImpl(ComuneRepository repository, AnimatoreService animatoreService, ContributorService contributorService, ContributorAutorizzatoService contributorAutorizzatoService, CuratoreService curatoreService, PoiService poiService, ItinerarioService itinerarioService, ContestService contestService) {
        this.repository = repository;
        this.animatoreService = animatoreService;
        this.contributorService = contributorService;
        this.contributorAutorizzatoService = contributorAutorizzatoService;
        this.curatoreService = curatoreService;
        this.poiService = poiService;
        this.itinerarioService = itinerarioService;
        this.contestService = contestService;
    }


    @Override
    public void deleteByNome(String nomeComune) {
        repository.deleteById(nomeComune);
    }


    public Comune save(Comune comune) {
        return repository.save(comune);
    }

    @Override
    public Comune creaComune(String nomeComune) throws ConnessioneFallitaException {
        return save(new Comune(nomeComune));
    }

    @Override
    public Optional<Comune> getByNome(String nomeComune) {
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
    public List<Itinerario> getItinerariNelComune(String nomeComune) {
        return itinerarioService.findByNomeComune(nomeComune);
    }
    @Override
    public List<Contest> getContestsNelComune(String nomeComune) {
        return contestService.getContestByComune(nomeComune);
    }
    @Override
    public List<Curatore> getCuratoriDelComune(String nome_comune) {
        return curatoreService.findByNomeComune(nome_comune);
    }


    @Override
    public List<PuntoInteresse> getPuntiInteresseNelComune(String nomeComune) throws IllegalArgumentException {
        Optional<Comune> oComune = getByNome(nomeComune);
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
