package ids.unicam.Service;

import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.Comune;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;

import java.util.List;
import java.util.Optional;

public interface ComuneService {
    

    List<Animatore> getAnimatoriDelComune(String nomeComune, String usernameGestore);

    List<Contributor> getContributorDelComune(String nomeComune, String usernameGestore);

    List<Contest> getContestsNelComune(String nomeComune);


    List<Curatore> getCuratoriDelComune(String nomeComune, String usernameGestore);

    List<PuntoInteresse> getPuntiInteresseNelComune(String nomeComune) throws IllegalArgumentException;

    List<Comune> findAll();

    Comune creaComune(String nomeComune, String usernameGestore) throws ConnessioneFallitaException,IllegalArgumentException;

    Optional<Comune> getByNome(String id);

    void deleteByNome(String nomeComune);

    List<ContributorAutorizzato> getContributorAutorizzatiDelComune(String nomeComune, String usernameGestore);

    List<Itinerario> getItinerariNelComune(String nomeComune);
}
