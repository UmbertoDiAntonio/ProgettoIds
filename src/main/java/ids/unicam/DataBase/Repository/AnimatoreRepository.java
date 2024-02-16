package ids.unicam.DataBase.Repository;

import ids.unicam.models.Comune;
import ids.unicam.models.users.organizzazioneComune.Animatore;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface AnimatoreRepository extends JpaRepository<Animatore, Integer> {
    List<Animatore> findByCognome(String cognome);

    List<Animatore> findByNome(String nome);

    List<Animatore> findByComune(Comune comune);

    List<Animatore> findByComuneNome(String nome_comune);
}
