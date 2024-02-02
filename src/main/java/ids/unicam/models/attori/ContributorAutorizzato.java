package ids.unicam.models.attori;

import ids.unicam.Comune;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Stato;
import jakarta.persistence.Entity;


@Entity
public class ContributorAutorizzato extends Contributor{
    protected ContributorAutorizzato(Comune comune, Contributor contributor) {
        super(comune,contributor);
    }

    public ContributorAutorizzato() {

    }

    /**
     * Se il punto di interesse si trova all'interno del territorio del comune del Contributor invia la richiesta di aggiunta al controller di contenuti associato al comune,
     * il punto di interesse è automaticamente approvato
     *
     * @param puntoInteresse il punto di interesse da aggiungere al comune del contributor
     * @return true se il punto di interesse è stato aggiunto, false se il punto non fa parte del comune
     */
    @Override
    public boolean aggiungiPuntoInteresse(PuntoInteresse puntoInteresse){
        if(super.aggiungiPuntoInteresse(puntoInteresse)){
            puntoInteresse.setStato(Stato.APPROVED);
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
    public boolean aggiungiMateriale(PuntoInteresse puntoInteresse, Materiale materiale){
        if(super.aggiungiMateriale(puntoInteresse,materiale)){
            materiale.setStato(Stato.APPROVED);
            return true;
        }
        return false;
    }
}