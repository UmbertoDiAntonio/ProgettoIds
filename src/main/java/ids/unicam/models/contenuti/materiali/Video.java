package ids.unicam.models.contenuti.materiali;

import ids.unicam.models.DTO.MaterialeDTO;
import jakarta.persistence.DiscriminatorValue;
import jakarta.persistence.Entity;
import lombok.NoArgsConstructor;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Base64;

@NoArgsConstructor
@Entity
@DiscriminatorValue("Video")
public class Video extends MaterialeGenerico {
    public Video(MaterialeDTO materialeDTO) {
        super(materialeDTO);
    }

    @Override
    public String getBase64() {
        File file = new File(super.getFile());
        byte[] videoBytes = new byte[(int) file.length()];

        try (FileInputStream fis = new FileInputStream(file)) {
            int bytesRead = fis.read(videoBytes);
            if (bytesRead == -1) {
                throw new IOException("Nessun byte letto");
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }


        return Base64.getEncoder().encodeToString(videoBytes);
    }

    @Override
    public String toString() {
        return super.toString();
    }
}
