package ids.unicam.models.contenuti.materiali;

import ids.unicam.models.DTO.MaterialeDTO;
import jakarta.persistence.DiscriminatorValue;
import jakarta.persistence.Entity;
import lombok.NoArgsConstructor;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.util.Base64;

@NoArgsConstructor
@Entity
@DiscriminatorValue("Foto")
public class Foto extends MaterialeGenerico {
    public Foto(MaterialeDTO materialeDTO) {
        super(materialeDTO);
    }

    @Override
    public String getBase64() {
        try {
            BufferedImage image = ImageIO.read(new File(super.getFile()));
            return imageToBase64String(image);

        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static String imageToBase64String(BufferedImage image) throws IOException {
        ByteArrayOutputStream os = new ByteArrayOutputStream();
        ImageIO.write(image, "jpg", os);
        return Base64.getEncoder().encodeToString(os.toByteArray());
    }

    @Override
    public String toString() {
        return "FOTO " + super.toString();
    }
}
