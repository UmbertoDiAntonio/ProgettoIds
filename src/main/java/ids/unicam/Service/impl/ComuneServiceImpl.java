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
    private final ComuneRepository comuneRepository;
    private final AnimatoreServiceImpl animatoreServiceImpl;
    private final ContributorServiceImpl contributorServiceImpl;
    private final ContributorAutorizzatoServiceImpl contributorAutorizzatoServiceImpl;
    private final CuratoreServiceImpl curatoreServiceImpl;
    private final PoiServiceImpl poiServiceImpl;

    @Autowired
    public ComuneServiceImpl(ComuneRepository comuneRepository, AnimatoreServiceImpl animatoreServiceImpl,
                             ContributorServiceImpl contributorServiceImpl,
                             ContributorAutorizzatoServiceImpl contributorAutorizzatoServiceImpl,
                             CuratoreServiceImpl curatoreServiceImpl, PoiServiceImpl poiServiceImpl) {
        this.comuneRepository = comuneRepository;
        this.animatoreServiceImpl = animatoreServiceImpl;
        this.contributorServiceImpl = contributorServiceImpl;
        this.contributorAutorizzatoServiceImpl = contributorAutorizzatoServiceImpl;
        this.curatoreServiceImpl = curatoreServiceImpl;
        this.poiServiceImpl = poiServiceImpl;
    }


    @Override
    public void deleteById(String nomeComune) {
        comuneRepository.deleteById(nomeComune);
    }


    public Comune save(Comune comune) {
        return comuneRepository.save(comune);
    }


    public Comune creaComune(Comune comune) {
        return save(comune);
    }



    @Override
    public Optional<Comune> findById(String nomeComune) {
        return comuneRepository.findById(nomeComune);
    }


    @Override
    public List<Comune> findAll() {
        return comuneRepository.findAll();
    }

    public void deleteAll() {
        comuneRepository.deleteAll();
    }

    @Override
    public List<Animatore> getAnimatoriDelComune(String nome_comune) {
        return animatoreServiceImpl.findByNomeComune(nome_comune);
    }

    @Override
    public List<Contributor> getContributorDelComune(String nome_comune) {
        return contributorServiceImpl.findByNomeComune(nome_comune);
    }

    @Override
    public List<ContributorAutorizzato> getContributorAutorizzatiDelComune(String nome_comune) {
        return contributorAutorizzatoServiceImpl.findByNomeComune(nome_comune);
    }

    @Override
    public List<Curatore> getCuratoriDelComune(String nome_comune) {
        return curatoreServiceImpl.findByNomeComune(nome_comune);
    }

    @Override
    public void rimuoviContributor(String id) {
        contributorServiceImpl.deleteById(id);
    }

    @Override
    public Optional<Comune> getComuneByNome(String nomeComune) throws IllegalArgumentException {
        return comuneRepository.findComuneByNomeIgnoreCase(nomeComune);
    }

    @Override
    public List<PuntoInteresse> getPuntiInteresseNelComune(String nomeComune) throws IllegalArgumentException {
        Optional<Comune> oComune = getComuneByNome(nomeComune);
        if (oComune.isPresent()) {
            Comune comune = oComune.get();
            List<PuntoInteresse> puntiInteresseNelComune = new ArrayList<>();
            List<PuntoInteresse> tuttiPuntiInteresse = poiServiceImpl.findActive();
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
