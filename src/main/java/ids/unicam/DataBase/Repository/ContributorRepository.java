package ids.unicam.DataBase.Repository;

import ids.unicam.models.Comune;
import ids.unicam.models.attori.Contributor;
import org.jetbrains.annotations.NotNull;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface ContributorRepository extends JpaRepository<Contributor,Integer> {
    @Query("SELECT e FROM Contributor e ORDER BY e.id DESC LIMIT 1")
    Contributor getLast();

    @Query("SELECT e FROM Contributor e ORDER BY e.id ASC LIMIT 1")
    Contributor getFirst();

    List<Contributor> findByCognome(String cognome);
    List<Contributor> findByNome(String nome);

    List<Contributor> findByComune(Comune comune);

    @Query("SELECT c.comune FROM Contributor c WHERE c.id = :id")
    Comune findComuneByContributorID(int id);
    @Query("SELECT c FROM Contributor c WHERE c.comune.nome = :nome_comune")
    List<Contributor> findByComuneNome(String nome_comune);

    @Override
    void deleteById(@NotNull Integer integer);
}
