package ids.unicam.models.Repository;

import ids.unicam.models.attori.TuristaAutenticato;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TuristaAutenticatoRepository extends CrudRepository <TuristaAutenticato,Integer>{

    @Query("SELECT e FROM TuristaAutenticato e ORDER BY e.id DESC LIMIT 1")
    public TuristaAutenticato getLast();

    @Query("SELECT e FROM TuristaAutenticato e ORDER BY e.id ASC LIMIT 1")
    public TuristaAutenticato getFirst();


}
