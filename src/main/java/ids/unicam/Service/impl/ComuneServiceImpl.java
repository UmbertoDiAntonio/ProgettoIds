package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.ComuneRepository;
import ids.unicam.Service.*;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.Comune;
import ids.unicam.models.attori.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

import static ids.unicam.Main.logger;

@Service
public class ComuneServiceImpl implements ComuneService {
    private final ComuneRepository repository;
    private final AnimatoreService animatoreService;
    private final ContributorService contributorService;
    private final ContributorAutorizzatoService contributorAutorizzatoService;
    private final CuratoreService curatoreService;
    private final GestorePiattaformaService gestorePiattaformaService;

    @Autowired
    public ComuneServiceImpl(ComuneRepository repository, AnimatoreService animatoreService, ContributorService contributorService, ContributorAutorizzatoService contributorAutorizzatoService, CuratoreService curatoreService, GestorePiattaformaService gestorePiattaformaService) {
        this.repository = repository;
        this.animatoreService = animatoreService;
        this.contributorService = contributorService;
        this.contributorAutorizzatoService = contributorAutorizzatoService;
        this.curatoreService = curatoreService;
        this.gestorePiattaformaService = gestorePiattaformaService;
    }


    /**
     * Cancella il comune con il nome indicato
     *
     * @param nomeComune il nome del comune
     */
    @Override
    public void deleteByNome(@NotNull String nomeComune, @NotNull String usernameGestore) {
        Optional<GestorePiattaforma> oGestore = gestorePiattaformaService.findByUsername(usernameGestore);
        if (oGestore.isPresent()) {
            repository.deleteById(nomeComune);
        } else {
            logger.error("username del gestore non valido");
            throw new IllegalArgumentException("username del gestore non valido");
        }
    }


    public @NotNull Comune save(@NotNull Comune comune) {
        return repository.save(comune);
    }

    /**
     * Crea un nuovo comune all'interno della piattaforma
     *
     * @param nomeComune      il nome del comune da creare
     * @param usernameGestore l'username di chi sta creando il comune (il gestore della piattaforma)
     * @return il comune creato
     * @throws ConnessioneFallitaException se non è possibile connettersi al sistema OSM
     */
    @Override
    public @NotNull Comune creaComune(@NotNull String nomeComune, @NotNull String usernameGestore) throws ConnessioneFallitaException {
        Optional<GestorePiattaforma> oGestore = gestorePiattaformaService.findByUsername(usernameGestore);
        if (oGestore.isPresent()) {
            return save(new Comune(nomeComune));
        } else {
            logger.error("Devi essere il gestore della Piattaforma per creare Comuni");
            throw new IllegalArgumentException("Devi essere il gestore della Piattaforma per creare Comuni");
        }
    }

    /**
     * Ottieni il comune con il nome indicato
     *
     * @param nomeComune il nome del comune da cercare
     * @return il comune con il nome inserito
     */
    @Override
    public @NotNull Optional<Comune> getByNome(@NotNull String nomeComune) {
        return repository.findById(nomeComune);
    }

    /**
     * Trova tutti i comuni presenti nella piattaforma
     *
     * @return una lista contenente tutti i comuni della piattaforma
     */
    @Override
    public @NotNull List<Comune> findAll() {
        return Collections.unmodifiableList(repository.findAll());
    }

    /**
     * Trova tutti i comuni nella piattaforma che rispettano la condizione
     *
     * @param predicate un espressione da valutare sui Comuni
     * @return la lista di comuni che rispettano la condizione
     */
    @Override
    public @NotNull List<Comune> find(@Nullable Predicate<Comune> predicate) {
        if (predicate != null) {
            List<Comune> list = new ArrayList<>();
            for (Comune comune : findAll())
                if (predicate.test(comune))
                    list.add(comune);
            return Collections.unmodifiableList(list);
        }
        return findAll();
    }

    /**
     * Trova tutti gli animatori associati al comune indicato
     *
     * @param nomeComune      il comune di cui si vogliono ottenere gli animatori
     * @param usernameGestore l'username di chi sta creando il comune (il gestore della piattaforma)
     * @return una lista contenente tutti gli animatori trovati
     */
    @Override
    public @NotNull List<Animatore> getAnimatoriDelComune(@NotNull String nomeComune, @NotNull String usernameGestore) throws IllegalArgumentException {
        if (!controllaGestore(usernameGestore)) {
            logger.error("Devi essere il gestore della Piattaforma per creare Comuni");
            throw new IllegalArgumentException("Devi essere il gestore della Piattaforma per creare Comuni");
        }
        return Collections.unmodifiableList(animatoreService.find(animatore -> animatore.getComune().getNome().equals(nomeComune)));
    }


    /**
     * Trova tutti i contributor associati al comune indicato
     *
     * @param nomeComune      il comune di cui si vogliono ottenere i contributor
     * @param usernameGestore l'username di chi sta creando il comune (il gestore della piattaforma)
     * @return una lista contenente tutti i contributor trovati
     */
    @Override
    public @NotNull List<Contributor> getContributorDelComune(@NotNull String nomeComune, @NotNull String usernameGestore) {
        if (controllaGestore(usernameGestore)) {
            return Collections.unmodifiableList(contributorService.find(contributor -> contributor.getComune().getNome().equals(nomeComune)));
        }
        logger.error("Devi essere il gestore della Piattaforma per creare Comuni");
        throw new IllegalArgumentException("Devi essere il gestore della Piattaforma per creare Comuni");
    }


    /**
     * Trova tutti i contributor autorizzati associati al comune indicato
     *
     * @param nomeComune      il comune di cui si vogliono ottenere i contributor autorizzati
     * @param usernameGestore l'username di chi sta creando il comune (il gestore della piattaforma)
     * @return una lista contenente tutti i contributor autorizzati trovati
     */
    @Override
    public @NotNull List<ContributorAutorizzato> getContributorAutorizzatiDelComune(@NotNull String nomeComune, @NotNull String usernameGestore) {
        if (!controllaGestore(usernameGestore)) {
            logger.error("Devi essere il gestore della Piattaforma per creare Comuni");
            throw new IllegalArgumentException("Devi essere il gestore della Piattaforma per creare Comuni");
        }
        return Collections.unmodifiableList(contributorAutorizzatoService.find(contributorAutorizzato -> contributorAutorizzato.getComune().getNome().equals(nomeComune)));
    }


    /**
     * Trova tutti i Curatori autorizzati associati al comune indicato
     *
     * @param nomeComune il comune di cui si vogliono ottenere i Curatori
     * @return una lista contenente tutti i Curatori trovati
     */
    @Override
    public @NotNull List<Curatore> getCuratoriDelComune(@NotNull String nomeComune, @NotNull String usernameGestore) {
        if (!controllaGestore(usernameGestore)) {
            logger.error("Devi essere il gestore della Piattaforma per creare Comuni");
            throw new IllegalArgumentException("Devi essere il gestore della Piattaforma per creare Comuni");
        }
        return Collections.unmodifiableList(curatoreService.find(curatore -> curatore.getComune().getNome().equals(nomeComune)));
    }

    private boolean controllaGestore(@NotNull String usernameGestore) {
        Optional<GestorePiattaforma> oGestore = gestorePiattaformaService.findByUsername(usernameGestore);
        return oGestore.isPresent();
    }

}
