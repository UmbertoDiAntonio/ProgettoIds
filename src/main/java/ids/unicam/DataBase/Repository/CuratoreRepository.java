package ids.unicam.DataBase.Repository;

import ids.unicam.models.attori.Curatore;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface CuratoreRepository extends JpaRepository<Curatore, String> {
    List<Curatore> findCuratoreByComuneNome(String nome_comune);


}
