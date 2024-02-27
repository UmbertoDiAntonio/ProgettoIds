package ids.unicam.Service;

import ids.unicam.models.Comune;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;

import java.util.List;
import java.util.Optional;

public interface ComuneService {
    Comune creaComune(Comune comune);
    List<Animatore> getAnimatoriDelComune(String nome_comune);

    List<Contributor> getContributorDelComune(String nome_comune);

    List<ContributorAutorizzato> getContributorAutorizzatiDelComune(String nome_comune);

    List<Curatore> getCuratoriDelComune(String nome_comune);

    void rimuoviContributor(String id);

    Optional<Comune> getComuneByNome(String nomeComune) throws IllegalArgumentException;

    List<PuntoInteresse> getPuntiInteresseNelComune(String nomeComune)  throws IllegalArgumentException;

    List<Comune> findAll();

    Optional<Comune> findById(String id);

    void deleteById(String nomeComune);

}
