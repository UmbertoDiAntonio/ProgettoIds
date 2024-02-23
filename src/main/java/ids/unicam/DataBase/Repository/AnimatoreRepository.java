package ids.unicam.DataBase.Repository;

import ids.unicam.models.Comune;
import ids.unicam.models.attori.Animatore;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface AnimatoreRepository extends JpaRepository<Animatore, String> {
    List<Animatore> findByCognome(String cognome);

    List<Animatore> findByComune(Comune comune);

    List<Animatore> findByComuneNome(String nome_comune);
}
