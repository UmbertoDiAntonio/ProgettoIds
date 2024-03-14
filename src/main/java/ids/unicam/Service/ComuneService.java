package ids.unicam.Service;

import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.Comune;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.attori.Curatore;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

public interface ComuneService {


    @NotNull List<Comune> find(@Nullable Predicate<Comune> predicate);

    @NotNull List<Animatore> getAnimatoriDelComune(@NotNull String nomeComune, @NotNull String usernameGestore);

    @NotNull List<Contributor> getContributorDelComune(@NotNull String nomeComune, @NotNull String usernameGestore);

    @NotNull List<Curatore> getCuratoriDelComune(@NotNull String nomeComune, @NotNull String usernameGestore);

    @NotNull List<Comune> findAll();

    @NotNull Comune creaComune(@NotNull String nomeComune, @NotNull String usernameGestore) throws ConnessioneFallitaException, IllegalArgumentException;

    @NotNull Optional<Comune> getByNome(@NotNull String id);

    void deleteByNome(@NotNull String nomeComune, @NotNull String usernameGestore);

    @NotNull List<ContributorAutorizzato> getContributorAutorizzatiDelComune(@NotNull String nomeComune, @NotNull String usernameGestore);
}
