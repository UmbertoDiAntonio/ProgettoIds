package ids.unicam.DataBase.Repository;

import ids.unicam.models.Invito;
import ids.unicam.models.users.TuristaAutenticato;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface InvitoRepository extends JpaRepository<Invito, Integer> {

    List<Invito> findByInvitato(TuristaAutenticato turistaAutenticato);

    @Query("SELECT i FROM TuristaAutenticato t JOIN Invito i on t.id = i.invitato.id WHERE t.id = :id")
    List<Invito> findInvitiByTurista(Integer id);
}
