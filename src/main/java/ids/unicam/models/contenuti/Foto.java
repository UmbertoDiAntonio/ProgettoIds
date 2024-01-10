package ids.unicam.models.contenuti;

import ids.unicam.models.attori.Contributor;

public class Foto extends  Materiale{
    public Foto(boolean approved, PuntoInteresse puntoInteresse, Contributor contributor) {
        super(approved,puntoInteresse,contributor);
    }
}
