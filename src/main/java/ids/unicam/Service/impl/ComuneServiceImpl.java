package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.ComuneRepository;
import ids.unicam.Service.*;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.Comune;
import ids.unicam.models.attori.*;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static ids.unicam.Main.logger;

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
    private final GestorePiattaformaService gestorePiattaformaService;

    @Autowired
    public ComuneServiceImpl(ComuneRepository repository, AnimatoreService animatoreService, ContributorService contributorService, ContributorAutorizzatoService contributorAutorizzatoService, CuratoreService curatoreService, PoiService poiService, ItinerarioService itinerarioService, ContestService contestService, GestorePiattaformaService gestorePiattaformaService) {
        this.repository = repository;
        this.animatoreService = animatoreService;
        this.contributorService = contributorService;
        this.contributorAutorizzatoService = contributorAutorizzatoService;
        this.curatoreService = curatoreService;
        this.poiService = poiService;
        this.itinerarioService = itinerarioService;
        this.contestService = contestService;
        this.gestorePiattaformaService = gestorePiattaformaService;
    }


    @Override
    public void deleteByNome(String nomeComune) {
        repository.deleteById(nomeComune);
    }


    public Comune save(Comune comune) {
        return repository.save(comune);
    }

    @Override
    public Comune creaComune(String nomeComune, String usernameCreatore) throws ConnessioneFallitaException {
        Optional<GestorePiattaforma> oGestore = gestorePiattaformaService.findByUsername(usernameCreatore);
        if(oGestore.isPresent()){
            return save(new Comune(nomeComune));
        }else {
            logger.error("Devi essere il gestore della Piattaforma per creare Comuni");
            throw new IllegalArgumentException("Devi essere il gestore della Piattaforma per creare Comuni");
        }
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
    public List<Animatore> getAnimatoriDelComune(String nomeComune,String usernameGestore) {
        Optional<GestorePiattaforma> oGestore = gestorePiattaformaService.findByUsername(usernameGestore);
        if(oGestore.isEmpty()) {
            logger.error("Devi essere il gestore della Piattaforma per creare Comuni");
            throw new IllegalArgumentException("Devi essere il gestore della Piattaforma per creare Comuni");
        }
        return animatoreService.findByNomeComune(nomeComune);
    }

    @Override
    public List<Contributor> getContributorDelComune(String nomeComune, String usernameGestore) {
        Optional<GestorePiattaforma> oGestore = gestorePiattaformaService.findByUsername(usernameGestore);
        if(oGestore.isEmpty()) {
            logger.error("Devi essere il gestore della Piattaforma per creare Comuni");
            throw new IllegalArgumentException("Devi essere il gestore della Piattaforma per creare Comuni");
        }
        return contributorService.findByNomeComune(nomeComune);
    }

    @Override
    public List<ContributorAutorizzato> getContributorAutorizzatiDelComune(String nomeComune, String usernameGestore) {
        Optional<GestorePiattaforma> oGestore = gestorePiattaformaService.findByUsername(usernameGestore);
        if(oGestore.isEmpty()) {
            logger.error("Devi essere il gestore della Piattaforma per creare Comuni");
            throw new IllegalArgumentException("Devi essere il gestore della Piattaforma per creare Comuni");
        }
        return contributorAutorizzatoService.findByNomeComune(nomeComune);
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
    public List<Curatore> getCuratoriDelComune(String nomeComune, String usernameGestore) {
        Optional<GestorePiattaforma> oGestore = gestorePiattaformaService.findByUsername(usernameGestore);
        if(oGestore.isEmpty()) {
            logger.error("Devi essere il gestore della Piattaforma per creare Comuni");
            throw new IllegalArgumentException("Devi essere il gestore della Piattaforma per creare Comuni");
        }
        return curatoreService.findByNomeComune(nomeComune);
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
