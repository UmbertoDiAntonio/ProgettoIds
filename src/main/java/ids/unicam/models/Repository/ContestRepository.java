package ids.unicam.models.Repository;

import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ContestRepository extends JpaRepository<Contest,Integer> {

    List<Contest> findContestByCreatore(Animatore animatore);

    List<Contest> findContestByPartecipantiContains(TuristaAutenticato turistaAutenticato);
}
