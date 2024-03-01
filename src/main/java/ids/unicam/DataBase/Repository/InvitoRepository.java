package ids.unicam.DataBase.Repository;

import ids.unicam.models.Invito;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface InvitoRepository extends JpaRepository<Invito, Integer> {

    @Query("SELECT i FROM TuristaAutenticato t JOIN Invito i on t.username = i.invitato.username WHERE t.username = :usernameTurista")
    List<Invito> findInvitiByTurista(String usernameTurista);
}
