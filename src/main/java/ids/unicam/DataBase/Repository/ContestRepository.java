package ids.unicam.DataBase.Repository;

import ids.unicam.models.Comune;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.Taggable;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface ContestRepository extends JpaRepository<Contest, Integer> {

    List<Contest> findContestByCreatore(Animatore animatore);

    @Query("SELECT t FROM Contest c JOIN c.partecipanti p JOIN TuristaAutenticato t on p.username = t.username WHERE c.id = :id ")
    List<TuristaAutenticato> findPartecipantiByContest(int id);

    @Query("select p.materiali from Contest p where p.id=:idContest")
    List<MaterialeGenerico> getMateriali(int idContest);

    Optional<Contest> findContestByMaterialiContaining(MaterialeGenerico materialeGenerico);

    @Query("select i from Contest i where i.comune.nome =:nomeComune")
    List<Contest> findContestByComune(String nomeComune);

}
