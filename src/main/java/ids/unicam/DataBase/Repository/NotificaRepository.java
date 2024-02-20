package ids.unicam.DataBase.Repository;

import ids.unicam.models.Notifica;
import ids.unicam.models.attori.TuristaAutenticato;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface NotificaRepository extends JpaRepository<Notifica, Integer> {


    List<Notifica> findByRicevente(TuristaAutenticato ricevente);

}
