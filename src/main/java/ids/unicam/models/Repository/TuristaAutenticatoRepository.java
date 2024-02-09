package ids.unicam.models.Repository;

import ids.unicam.models.attori.TuristaAutenticato;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface TuristaAutenticatoRepository extends JpaRepository<TuristaAutenticato,Integer> {
    List<TuristaAutenticato> findByCognome(String cognome);
    List<TuristaAutenticato> findByNome(String nome);

}
