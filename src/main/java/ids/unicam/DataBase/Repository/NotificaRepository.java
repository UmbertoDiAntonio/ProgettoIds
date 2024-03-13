package ids.unicam.DataBase.Repository;

import ids.unicam.models.contenuti.notifiche.Notifica;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface NotificaRepository extends JpaRepository<Notifica, Integer> {
    List<Notifica> findByUsernameDestinatario(String usernameDestinatario);

    void deleteByUsernameDestinatario(String usernameTurista);
}
