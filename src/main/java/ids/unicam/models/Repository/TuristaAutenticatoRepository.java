package ids.unicam.models.Repository;

import ids.unicam.models.attori.TuristaAutenticato;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TuristaAutenticatoRepository extends CrudRepository <TuristaAutenticato,Integer>{
}
