package ids.unicam.models.contenuti.materiali;

import ids.unicam.models.DTO.TuristaAutenticatoDTO;
import jakarta.persistence.DiscriminatorValue;
import jakarta.persistence.Entity;
import lombok.NoArgsConstructor;

import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.UnsupportedAudioFileException;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.util.Base64;

@NoArgsConstructor
@Entity
@DiscriminatorValue("Audio")
public class Audio extends MaterialeGenerico {
    public Audio(String filePath, TuristaAutenticatoDTO turistaAutenticatoDTO) {
        super(filePath,turistaAutenticatoDTO);
    }

    public static byte[] audioToByteArray(AudioInputStream audioInputStream) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        byte[] buffer = new byte[4096];
        int bytesRead;
        while ((bytesRead = audioInputStream.read(buffer)) != -1) {
            baos.write(buffer, 0, bytesRead);
        }
        return baos.toByteArray();
    }

    public static String byteArrayToBase64String(byte[] bytes) {
        return Base64.getEncoder().encodeToString(bytes);
    }

    @Override
    public String getBase64() {
        try {
            AudioInputStream audioInputStream = AudioSystem.getAudioInputStream(new File(super.getFile()));
            byte[] audioBytes = audioToByteArray(audioInputStream);
            return byteArrayToBase64String(audioBytes);
        } catch (UnsupportedAudioFileException | IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public String toString() {
        return super.toString();
    }
}
