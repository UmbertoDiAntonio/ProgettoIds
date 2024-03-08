package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.PoiRepository;
import ids.unicam.Service.*;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.Comune;
import ids.unicam.models.Punto;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.puntiInteresse.*;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static ids.unicam.Main.logger;

@Service
public class PoiServiceImpl implements PoiService {
    private final PoiRepository repository;
    private final TagService tagService;
    private final ContributorService contributorService;
    private final TuristaAutenticatoService turistaAutenticatoService;
    private final MaterialeServiceImpl materialeService;

    @Autowired
    public PoiServiceImpl(PoiRepository repository, TagService tagService, ContributorService contributorService, TuristaAutenticatoService turistaAutenticatoService, MaterialeServiceImpl materialeService) {
        this.repository = repository;
        this.tagService = tagService;
        this.contributorService = contributorService;
        this.turistaAutenticatoService = turistaAutenticatoService;
        this.materialeService = materialeService;
    }


    @Override
    public void deleteById(int id) {
        repository.deleteById(id);
    }

    @Transactional
    @Override
    public void modificaScadenza(String usernameContributor, int idPuntoInteresse, LocalDate expireDate) throws IllegalArgumentException {
        Optional<Contributor> oContributor = contributorService.getByUsername(usernameContributor);
        if (oContributor.isPresent()) {
            Contributor contributor = oContributor.get();
            Optional<PuntoInteresse> oPoi = findById(idPuntoInteresse);
            if (oPoi.isPresent()) {
                PuntoInteresse puntoInteresse = oPoi.get();
                if (contributor.getComune().equals(puntoInteresse.getComune())) {
                    if (!expireDate.isAfter(LocalDate.now())) {
                        logger.error("La scadenza deve essere una data futura");
                        throw new IllegalArgumentException("La scadenza deve essere una data futura");
                    }
                    puntoInteresse.setExpireDate(expireDate);
                    save(puntoInteresse);
                } else {
                    logger.error("punto di interesse fuori dal comune di appartenenza del contributor");
                    throw new IllegalArgumentException("punto di interesse fuori dal comune di appartenenza del contributor");
                }
            } else {
                logger.error("id del punto di interesse non esiste");
                throw new IllegalArgumentException("id del punto di interesse non esiste");
            }
        } else {
            logger.error("username del contributor non esiste");
            throw new IllegalArgumentException("username del contributor non esiste");
        }
    }

    @Override
    public Stato getStato(int idPunto) throws IllegalArgumentException {
        Optional<Stato> oStato = repository.getStatoById(idPunto);
        if (oStato.isEmpty())
            throw new IllegalArgumentException("id punto non valido");
        return oStato.get();
    }

    @Transactional
    @Override
    public PuntoInteresse creaPuntoInteresse(String nomePOI, Punto punto,Orario orario,TipologiaPuntoInteresse tipologiaPuntoInteresse, String usernameCreatore ) throws FuoriComuneException,IllegalArgumentException {
        Optional<Contributor> oContributor = contributorService.getByUsername(usernameCreatore);
        if (oContributor.isPresent()) {
            Contributor contributor = oContributor.get();
            if (!contributor.getComune().verificaCoordinateComune(punto)) {
                throw new FuoriComuneException("Posizione Punto di Interesse Fuori dall'area del comune");
            }
            PuntoInteresse puntoInteresse = new PuntoInteresse(nomePOI, punto, orario, tipologiaPuntoInteresse, contributor);
            puntoInteresse.setStato(contributor instanceof ContributorAutorizzato ? Stato.APPROVATO : Stato.IN_ATTESA);

            return save(puntoInteresse);
        } else {
            logger.error("username non valido");
            throw new IllegalArgumentException("username non valido");
        }
    }




    @Override
    public LocalDate getScadenza(int idPunto) throws IllegalArgumentException {
        Optional<LocalDate> oScadenza = repository.getExpireDateById(idPunto);
        if (oScadenza.isEmpty())
            throw new IllegalArgumentException("id punto non valido");
        return oScadenza.get();
    }

    @Transactional
    @Override
    public void aggiungiMateriale(String usernameTurista, int idPuntoInteresse, MaterialeGenerico materialeGenerico) throws IllegalArgumentException,IllegalStateException {
        Optional<TuristaAutenticato> oTurista = turistaAutenticatoService.getByUsername(usernameTurista);
        if (oTurista.isEmpty()) {
            throw new IllegalArgumentException("username non valido");
        }
        TuristaAutenticato turistaAutenticato = oTurista.get();

        Optional<PuntoInteresse> oPunto = getById(idPuntoInteresse);
        if (oPunto.isEmpty()) {
            throw new IllegalArgumentException("id punto interesse non valido");
        }
        PuntoInteresse puntoInteresse = oPunto.get();

        if (turistaAutenticato instanceof Contributor contributor) {
            if (!contributor.getComune().equals(puntoInteresse.getComune())) {
                logger.error("il contributor cerca di caricare il materiale fuori dal suo comune");
                throw new IllegalStateException("il contributor cerca di caricare il materiale fuori dal suo comune");
            }
        }
        if (Boolean.FALSE.equals(puntoInteresse.getStato().asBoolean())) {
            logger.error("il contributor cerca di caricare il materiale su un punto di interesse non approvato");
            throw new IllegalStateException("il contributor cerca di caricare il materiale su un punto di interesse non approvato");
        }
        materialeService.aggiungiMateriale(puntoInteresse, materialeGenerico);
        save(puntoInteresse);
    }


    @Override
    @Transactional
    public PuntoInteresse save(PuntoInteresse puntoInteresse) {
        return repository.save(puntoInteresse);
    }

    @Override
    public Optional<PuntoInteresse> findById(int id) {
        return repository.findById(id);
    }


    @Transactional
    @Override
    public List<PuntoInteresse> findActive() {
        return repository.findAll().stream()
                .filter(puntoInteresse -> !puntoInteresse.isExpired() || Boolean.TRUE.equals(puntoInteresse.getStato().asBoolean()))
                .collect(Collectors.toList());
    }


    @Transactional
    @Override
    public void aggiungiTag(int idPuntoInteresse, Tag tag,String usernameContributor) throws FuoriComuneException,IllegalArgumentException,IllegalStateException {
        Optional<Contributor> oContributor = contributorService.getByUsername(usernameContributor);
        if (oContributor.isEmpty()) {
            throw new IllegalArgumentException("username non valido");
        }
        Contributor contributor = oContributor.get();
        //TODO

        Optional<PuntoInteresse> oPuntoInteresse = getById(idPuntoInteresse);
        if (oPuntoInteresse.isPresent()) {
            PuntoInteresse puntoInteresse = oPuntoInteresse.get();
            if (tagService.haveTag(puntoInteresse, tag)) {
                logger.warn("Tag già aggiunto");
                return;
            }
            if(contributor.getComune()!=puntoInteresse.getComune()){
                throw new FuoriComuneException(contributor.getUsername()+" non può operare fuori dal suo comune");
            }
            if (!puntoInteresse.isExpired()) {
                tagService.aggiungiTag(puntoInteresse, tag);
                throw new IllegalStateException("Il Punto di interesse è scaduto");
            }
            save(puntoInteresse);
        } else {
            logger.error("L'id del punto di interesse non e' valido");
            throw new IllegalArgumentException("L'id del punto di interesse non e' valido");
        }
    }

    @Transactional
    @Override
    public void rimuoviTag(int idPuntoInteresse, Tag tag,String usernameContributor) throws FuoriComuneException {
        Optional<Contributor> oContributor = contributorService.getByUsername(usernameContributor);
        if (oContributor.isEmpty()) {
            throw new IllegalArgumentException("username non valido");
        }
        Contributor contributor = oContributor.get();
        //TODO

        Optional<PuntoInteresse> oPuntoInteresse = getById(idPuntoInteresse);
        if (oPuntoInteresse.isPresent()) {
            PuntoInteresse puntoInteresse = oPuntoInteresse.get();
            if (!tagService.haveTag(puntoInteresse, tag)) {
                return;
            }
            if(contributor.getComune()!=puntoInteresse.getComune()){
                throw new FuoriComuneException(contributor.getUsername()+" non può operare fuori dal suo comune");
            }
            if (!puntoInteresse.isExpired())
                tagService.rimuoviTag(puntoInteresse, tag);

            save(puntoInteresse);
        } else {
            logger.error("L'id del punto di interesse non e' valido");
            throw new IllegalArgumentException("L'id del punto di interesse non e' valido");
        }
    }

    @Override
    public List<Taggable> findByTag(String tag) {
        return repository.findByTagsValoreContaining(tag);
    }

    @Override
    public List<Tag> getTags(PuntoInteresse puntoInteresse) {
        return repository.getTags(puntoInteresse.getId());
    }

    @Override
    public Optional<PuntoInteresse> getById(int id) {
        return repository.findById(id);
    }

    @Override
    public Set<MaterialeGenerico> getMaterialiPoi(int idPunto) throws IllegalArgumentException {
        Optional<PuntoInteresse> oPuntoInteresse = getById(idPunto);
        if (oPuntoInteresse.isPresent()) {
            PuntoInteresse puntoInteresse = oPuntoInteresse.get();
            return puntoInteresse.getMateriali();
        } else {
            logger.error("L'id del punto di interesse non e' valido");
            throw new IllegalArgumentException("L'id del punto di interesse non e' valido");
        }
    }

    @Transactional
    @Override
    public List<String> getAsList() {
        List<PuntoInteresse> list = findActive();
        return getAsList(list);
    }


    @Override
    public List<String> getAsList(List<PuntoInteresse> preferiti) {
        List<String> result = new ArrayList<>();
        int i = 0;
        for (PuntoInteresse el : preferiti) {
            i++;
            result.add(i + ". " + el.mostraInformazioniGeneriche());
        }
        return result;
    }

    @Transactional
    @Override
    public List<String> getAsListDetailed() {
        List<PuntoInteresse> list = findActive();
        List<String> result = new ArrayList<>();
        int i = 0;
        for (PuntoInteresse el : list) {
            i++;
            result.add(i + ". " + el.mostraInformazioniDettagliate());
        }
        return result;
    }

    @Override
    public List<PuntoInteresse> findAll() {
        return repository.findAll();
    }

    @Transactional
    @Override
    public void setOrario(int idPunto, Orario.OrarioApertura orario, DayOfWeek day) throws IllegalArgumentException {
        Optional<PuntoInteresse> oPuntoInteresse = getById(idPunto);
        if (oPuntoInteresse.isPresent()) {
            PuntoInteresse puntoInteresse = oPuntoInteresse.get();
            puntoInteresse.getOrario().setOrarioApertura(day, orario);
            save(puntoInteresse);
        } else {
            logger.error("L'id del punto di interesse non e' valido");
            throw new IllegalArgumentException("L'id del punto di interesse non e' valido");
        }
    }


    @Override
    public Optional<PuntoInteresse> getPoiContainingMaterial(MaterialeGenerico materialeGenerico) {
        return repository.findPuntoInteresseByMaterialiContaining(materialeGenerico);
    }

    @Override
    public List<PuntoInteresse> getPoiByComune(Comune comune) {
        return repository.findPoiByComune(comune);
    }

    @Override
    public void deleteIfIsExpired(PuntoInteresse puntoInteresse) {
        if (puntoInteresse.isExpired()) {
            puntoInteresse.getMateriali().clear();
            deleteById(puntoInteresse.getId());
        }
    }
}
