package ids.unicam.Service;

import ids.unicam.models.Comune;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.Contributor;
import ids.unicam.models.attori.ContributorAutorizzato;
import ids.unicam.models.attori.Curatore;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public interface ComuneService {
    Comune creaComune(String nomeComune);
    List<Animatore> getAnimatoriDelComune(String nome_comune);

    List<Contributor> getContributorDelComune(String nome_comune);

    List<ContributorAutorizzato> getContributorAutorizzatiDelComune(String nome_comune);

    List<Curatore> getCuratoriDelComune(String nome_comune);

    void rimuoviContributor(int id);

    @Nullable Comune getComuneByNome(String nomeComune);

    List<PuntoInteresse> getPuntiInteresseNelComune(String nomeComune);
}
