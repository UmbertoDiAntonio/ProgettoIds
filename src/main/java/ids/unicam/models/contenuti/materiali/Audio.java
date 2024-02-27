package ids.unicam.models.contenuti.materiali;

import ids.unicam.models.DTO.MaterialeDTO;
import jakarta.persistence.DiscriminatorValue;
import jakarta.persistence.Entity;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Entity
@DiscriminatorValue("Audio")
public class Audio extends MaterialeGenerico {
    public Audio(MaterialeDTO materialeDTO) {
        super(materialeDTO);
    }

    @Override
    public String get() {
        return "Audio: "+getId()+", creata da "+ getCreatore().getNome();
    }

    @Override
    public String toString() {
        return super.toString();
    }
}
