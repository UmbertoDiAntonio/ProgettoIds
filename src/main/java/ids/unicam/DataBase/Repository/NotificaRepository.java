package ids.unicam.DataBase.Repository;

import ids.unicam.models.contenuti.notifiche.Notifica;
import org.jetbrains.annotations.NotNull;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface NotificaRepository extends JpaRepository<Notifica, Integer> {
    @NotNull List<Notifica> findByUsernameDestinatario(@NotNull String usernameDestinatario);

    void deleteByUsernameDestinatario(@NotNull String usernameTurista);
}
