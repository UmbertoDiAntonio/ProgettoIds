package ids.unicam.DataBase.Repository;

import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.materiali.MaterialeGenerico;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ContestRepository extends JpaRepository<Contest,Integer> {

    List<Contest> findContestByCreatore(Animatore animatore);

    List<Contest> findContestByPartecipantiContains(TuristaAutenticato turistaAutenticato);

    @Query("SELECT t FROM Contest c JOIN c.partecipanti p JOIN TuristaAutenticato t on p.id = t.id WHERE c.id = :id ")
    List<TuristaAutenticato> findPartecipantiByContest(int id);

    @Query("select p.listaMateriali from Contest p where p.id=:idContest")
    List<MaterialeGenerico> getMateriali(int idContest);
}