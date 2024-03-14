package ids.unicam.DataBase.Repository;

import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import org.jetbrains.annotations.NotNull;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface TuristaAutenticatoRepository extends JpaRepository<TuristaAutenticato, String> {
    @Query("SELECT t.preferiti FROM TuristaAutenticato t WHERE t.username = :usernameTurista")
    @NotNull List<PuntoInteresse> findPreferitiByTurista(@Param("usernameTurista") String usernameTurista);

    @Query("SELECT COUNT(*) FROM TuristaAutenticato t WHERE t.username=:username ")
    int countUsername(@NotNull String username);
}
