package ids.unicam.Service;

import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.Comune;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;

import java.util.List;
import java.util.Optional;

public interface ComuneService {

    List<Animatore> getAnimatoriDelComune(String nome_comune);

    List<Contributor> getContributorDelComune(String nome_comune);

    List<ContributorAutorizzato> getContributorAutorizzatiDelComune(String nome_comune);

    List<Curatore> getCuratoriDelComune(String nome_comune);

    List<PuntoInteresse> getPuntiInteresseNelComune(String nomeComune) throws IllegalArgumentException;

    List<Comune> findAll();

    Comune creaComune(String nomeComune) throws ConnessioneFallitaException;

    Optional<Comune> getByNome(String id);

    void deleteByNome(String nomeComune);

}
