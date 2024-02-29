package ids.unicam.DataBase.Repository;

import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface TuristaAutenticatoRepository extends JpaRepository<TuristaAutenticato, String> {
    @Query("SELECT DISTINCT t FROM TuristaAutenticato t JOIN FETCH t.preferiti")
    List<TuristaAutenticato> findTuristiConPreferiti();

    @Query("SELECT t.preferiti FROM TuristaAutenticato t WHERE t.username = :usernameTurista")
    List<PuntoInteresse> findPreferitiByTurista(@Param("usernameTurista") String usernameTurista);

    Optional<TuristaAutenticato> findByUsername(String username);

    @Query("SELECT COUNT(*) FROM TuristaAutenticato t WHERE t.username=:username ")
    Integer countUsername(String username);
}
