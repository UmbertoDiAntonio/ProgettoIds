package ids.unicam.DataBase.Repository;

import ids.unicam.models.contenuti.puntiInteresse.Tag;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface TagRepository extends JpaRepository<Tag,Integer> {
}
