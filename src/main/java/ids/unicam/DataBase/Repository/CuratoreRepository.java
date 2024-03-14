package ids.unicam.DataBase.Repository;

import ids.unicam.models.attori.Curatore;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface CuratoreRepository extends JpaRepository<Curatore, String> {

}
