package ids.unicam.models.contenuti.materiali;

import ids.unicam.models.DTO.TuristaAutenticatoDTO;
import jakarta.persistence.DiscriminatorValue;
import jakarta.persistence.Entity;
import lombok.NoArgsConstructor;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

@NoArgsConstructor
@Entity
@DiscriminatorValue("Testo")
public class Testo extends MaterialeGenerico {
    public Testo(String filePath, TuristaAutenticatoDTO turistaAutenticatoDTO) {
        super(filePath, turistaAutenticatoDTO);
    }

    private String readFileToString(String filePath) throws IOException {
        StringBuilder contentBuilder = new StringBuilder();
        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = br.readLine()) != null) {
                contentBuilder.append(line).append("\n");
            }
        }
        return contentBuilder.toString();
    }

    @Override
    public String getBase64() {
        try {
            return readFileToString(super.getFile());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public String toString() {
        return super.toString();
    }
}
