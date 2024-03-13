package ids.unicam.Service;

import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.Comune;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.attori.Curatore;

import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

public interface ComuneService {


    List<Comune> find(Predicate<Comune> predicate);

    List<Animatore> getAnimatoriDelComune(String nomeComune, String usernameGestore);

    List<Contributor> getContributorDelComune(String nomeComune, String usernameGestore);

    List<Curatore> getCuratoriDelComune(String nomeComune, String usernameGestore);

    List<Comune> findAll();

    Comune creaComune(String nomeComune, String usernameGestore) throws ConnessioneFallitaException, IllegalArgumentException;

    Optional<Comune> getByNome(String id);

    void deleteByNome(String nomeComune);

    List<ContributorAutorizzato> getContributorAutorizzatiDelComune(String nomeComune, String usernameGestore);
}
