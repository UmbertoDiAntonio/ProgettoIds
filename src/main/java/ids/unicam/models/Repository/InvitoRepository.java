package ids.unicam.models.Repository;

import ids.unicam.models.Invito;
import ids.unicam.models.attori.TuristaAutenticato;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface InvitoRepository extends JpaRepository<Invito, Integer> {

    List<Invito> findByInvitato(TuristaAutenticato turistaAutenticato);
}
