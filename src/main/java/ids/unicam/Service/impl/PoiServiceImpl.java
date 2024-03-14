package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.PoiRepository;
import ids.unicam.Service.ContributorService;
import ids.unicam.Service.PoiService;
import ids.unicam.Service.TagService;
import ids.unicam.exception.FuoriComuneException;
import ids.unicam.models.Punto;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.contenuti.Stato;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import ids.unicam.models.contenuti.puntiInteresse.DayOfWeek;
import ids.unicam.models.contenuti.puntiInteresse.Orario;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import ids.unicam.models.contenuti.puntiInteresse.TipologiaPuntoInteresse;
import jakarta.transaction.Transactional;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static ids.unicam.Main.logger;

@Service
public class PoiServiceImpl implements PoiService {
    private final PoiRepository repository;
    private final TagService tagService;
    private final ContributorService contributorService;


    @Autowired
    public PoiServiceImpl(PoiRepository repository, TagService tagService, ContributorService contributorService) {
        this.repository = repository;
        this.tagService = tagService;
        this.contributorService = contributorService;
    }


    @Override
    public void deleteById(int id) {
        repository.deleteById(id);
    }


    @Transactional
    @Override
    public void modificaScadenza(@NotNull String usernameContributor, int idPuntoInteresse, @NotNull LocalDate expireDate) throws IllegalArgumentException {
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
    public @NotNull Stato getStato(int idPunto) throws IllegalArgumentException {
        Optional<Stato> oStato = repository.getStatoById(idPunto);
        if (oStato.isEmpty())
            throw new IllegalArgumentException("id punto non valido");
        return oStato.get();
    }

    @Transactional
    @Override
    public @NotNull PuntoInteresse creaPuntoInteresse(@NotNull String nomePOI, @NotNull Punto punto, @NotNull Orario orario, @NotNull TipologiaPuntoInteresse tipologiaPuntoInteresse, @NotNull String usernameCreatore) throws FuoriComuneException, IllegalArgumentException {
        Optional<Contributor> oContributor = contributorService.getByUsername(usernameCreatore);
        if (oContributor.isPresent()) {
            Contributor contributor = oContributor.get();
            if (!contributor.getComune().verificaCoordinateComune(punto)) {
                throw new FuoriComuneException("Posizione Punto di Interesse Fuori dall'area del comune");
            }
            PuntoInteresse puntoInteresse = new PuntoInteresse(nomePOI, punto, orario, tipologiaPuntoInteresse, contributor);
            puntoInteresse.setStato(contributor instanceof ContributorAutorizzato ? Stato.APPROVATO : Stato.IN_ATTESA);

         /*
         for(Curatore curatore:comuneService.getCuratoriDelComune(puntoInteresse.getComune().getNome()))//TODO Diagramma
                notificaService.creaNotificaCreazionePoi(puntoInteresse,curatore);


          */
            return save(puntoInteresse);
        } else {
            logger.error("username non valido");
            throw new IllegalArgumentException("username non valido");
        }
    }


    @Override
    public @NotNull LocalDate getScadenza(int idPunto) throws IllegalArgumentException {
        Optional<LocalDate> oScadenza = repository.getExpireDateById(idPunto);
        if (oScadenza.isEmpty())
            throw new IllegalArgumentException("id punto non valido");
        return oScadenza.get();
    }


    @Override
    @Transactional
    public @NotNull PuntoInteresse save(@NotNull PuntoInteresse puntoInteresse) {
        return repository.save(puntoInteresse);
    }

    @Override
    public @NotNull Optional<PuntoInteresse> findById(int id) {
        return repository.findById(id);
    }


    @Transactional
    @Override
    public @NotNull List<PuntoInteresse> findActive() {
        return repository.findAll().stream()
                .filter(puntoInteresse -> !puntoInteresse.isExpired() || Boolean.TRUE.equals(puntoInteresse.getStato().asBoolean()))
                .collect(Collectors.toList());
    }


    @Transactional
    @Override
    public void aggiungiTag(int idPuntoInteresse, @NotNull String tag, @NotNull String usernameContributor) throws FuoriComuneException, IllegalArgumentException, IllegalStateException {
        Optional<Contributor> oContributor = contributorService.getByUsername(usernameContributor);
        if (oContributor.isEmpty()) {
            throw new IllegalArgumentException("username non valido");
        }
        Contributor contributor = oContributor.get();

        Optional<PuntoInteresse> oPuntoInteresse = getById(idPuntoInteresse);
        if (oPuntoInteresse.isPresent()) {
            PuntoInteresse puntoInteresse = oPuntoInteresse.get();
            if (tagService.haveTag(puntoInteresse, tag)) {
                logger.warn("Tag già aggiunto");
                return;
            }
            if (contributor.getComune() != puntoInteresse.getComune()) {
                throw new FuoriComuneException(contributor.getUsername() + " non può operare fuori dal suo comune");
            }
            if (!puntoInteresse.isExpired()) {
                tagService.aggiungiTag(puntoInteresse, tag);
            } else {
                logger.error("Il Punto di interesse è scaduto");
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
    public void rimuoviTag(int idPuntoInteresse, @NotNull String tag, @NotNull String usernameContributor) throws FuoriComuneException, IllegalArgumentException {
        Optional<Contributor> oContributor = contributorService.getByUsername(usernameContributor);
        if (oContributor.isEmpty()) {
            throw new IllegalArgumentException("username non valido");
        }
        Contributor contributor = oContributor.get();

        Optional<PuntoInteresse> oPuntoInteresse = getById(idPuntoInteresse);
        if (oPuntoInteresse.isPresent()) {
            PuntoInteresse puntoInteresse = oPuntoInteresse.get();
            if (!tagService.haveTag(puntoInteresse, tag)) {
                return;
            }
            if (contributor.getComune() != puntoInteresse.getComune()) {
                throw new FuoriComuneException(contributor.getUsername() + " non può operare fuori dal suo comune");
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
    public @NotNull List<PuntoInteresse> find(@Nullable Predicate<PuntoInteresse> predicate) {
        if (predicate == null)
            return findAll();
        List<PuntoInteresse> list = new ArrayList<>();
        for (PuntoInteresse puntoInteresse : findAll()) {
            if (predicate.test(puntoInteresse))
                list.add(puntoInteresse);
        }
        return Collections.unmodifiableList(list);
    }


    @Override
    public @NotNull List<String> getTags(int idPunto) {
        return repository.getTags(idPunto);
    }

    @Override
    public @NotNull Optional<PuntoInteresse> getById(int id) {
        return repository.findById(id);
    }


    @Override
    public @NotNull Set<MaterialeGenerico> getMaterialiPoi(int idPunto) throws IllegalArgumentException {
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
    public @NotNull List<String> getAsList() {
        List<PuntoInteresse> list = findActive();
        return getAsList(list);
    }

    @Override
    public @NotNull List<String> getAsList(@NotNull List<PuntoInteresse> preferiti) {
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
    public @NotNull List<String> getAsListDetailed() {
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
    public @NotNull List<PuntoInteresse> findAll() {
        return repository.findAll();
    }


    @Transactional
    @Override
    public void setOrario(int idPunto, @NotNull Orario.OrarioApertura orario, @NotNull DayOfWeek day) throws IllegalArgumentException {
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
    public @NotNull Optional<PuntoInteresse> getPoiContainingMaterial(@NotNull MaterialeGenerico materialeGenerico) {
        return repository.findPuntoInteresseByMaterialiContaining(materialeGenerico);
    }


    @Override
    public void deleteIfIsExpired(@NotNull PuntoInteresse puntoInteresse) {
        if (puntoInteresse.isExpired()) {
            puntoInteresse.getMateriali().clear();
            deleteById(puntoInteresse.getId());
        }
    }
}
