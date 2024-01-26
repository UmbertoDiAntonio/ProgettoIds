package ids.unicam.models.attori;

import ids.unicam.models.Comune;
import ids.unicam.models.contenuti.Itinerario;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.PuntoInteresse;

public class ContributorTrusted extends Contributor{
    protected ContributorTrusted(Comune comune,Contributor contributor) {
        super(comune,contributor);
    }

    /**
     * Se il punto di interesse si trova all'interno del territorio del comune del Contributor invia la richiesta di aggiunta al controller di contenuti associato al comune,
     * il punto di interesse è automaticamente approvato
     *
     * @param puntoInteresse il punto di interesse da aggiungere al comune del contributor
     * @return true se il punto di interesse è stato aggiunto, false se il punto non fa parte del comune
     */
    @Override
    public boolean addPuntoInteresse(PuntoInteresse puntoInteresse){
        if(super.addPuntoInteresse(puntoInteresse)){
            puntoInteresse.setApproved(true);
            return true;
        }
        return false;
    }

    /**
     * Se il punto di interesse si trova all'interno del territorio del comune del Contributor invia la richiesta di aggiunta al controller di contenuti associato al comune,
     * il materiale è automaticamente approvato
     *
     * @param puntoInteresse il punto di interesse del comune in cui aggiungere il materiale
     * @param materiale il materiale da aggiungere
     * @return true se il punto di interesse è stato aggiunto, false se il punto non fa parte del comune
     */
    @Override
    public boolean addMateriale(PuntoInteresse puntoInteresse, Materiale materiale){
        if(super.addMateriale(puntoInteresse,materiale)){
            materiale.setApproved(true);
            return true;
        }
        return false;
    }
}