package ids.unicam.DataBase.Repository;

import ids.unicam.models.Comune;
import ids.unicam.models.attori.Curatore;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface CuratoreRepository  extends JpaRepository<Curatore,String> {

    List<Curatore> findByCognome(String cognome);
    List<Curatore> findByNome(String nome);

    List<Curatore> findByComune(Comune comune);
    @Query("SELECT c FROM Curatore c WHERE c.comune.nome = :nome_comune")
    List<Curatore> findCuratoreByComuneNome(String nome_comune);


}
