package ids.unicam.models.Repository;

import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.PuntoInteresse;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface TuristaAutenticatoRepository extends JpaRepository<TuristaAutenticato,Integer> {
    List<TuristaAutenticato> findByCognome(String cognome);
    List<TuristaAutenticato> findByNome(String nome);

    @Query("SELECT DISTINCT t FROM TuristaAutenticato t JOIN FETCH t.preferiti")
    List<TuristaAutenticato> findTuristiConPreferiti();

    @Query("SELECT t.preferiti FROM TuristaAutenticato t WHERE t.id = :idTurista")
    List<PuntoInteresse> findPreferitiByTurista(@Param("idTurista") int idTurista);

    Optional<TuristaAutenticato> findByUsername(String username);

}
