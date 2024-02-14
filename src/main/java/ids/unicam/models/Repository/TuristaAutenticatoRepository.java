package ids.unicam.models.Repository;

import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.PuntoInteresse;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface TuristaAutenticatoRepository extends JpaRepository<TuristaAutenticato,Integer> {
    List<TuristaAutenticato> findByCognome(String cognome);
    List<TuristaAutenticato> findByNome(String nome);

    @Query("SELECT DISTINCT t FROM TuristaAutenticato t JOIN FETCH t.preferiti")
    List<TuristaAutenticato> findTuristiConPreferiti();

    List<PuntoInteresse> getPreferiti(TuristaAutenticato turistaAutenticato);
}