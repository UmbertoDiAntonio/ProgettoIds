package ids.unicam.models.attori;

import ids.unicam.models.Comune;
import ids.unicam.models.contenuti.Contenuto;
import ids.unicam.models.contenuti.Materiale;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Observer;

import java.util.ArrayList;

public class Curatore extends ContributorTrusted {

    ArrayList<Observer> osservatori = new ArrayList<>();

    public void aggiungiOsservatore(Observer osservatore) {
        osservatori.add(osservatore);
    }

    public void rimuoviOsservatore(Observer osservatore) {
        osservatori.remove(osservatore);
    }

    public void notifica(Boolean eventType, PuntoInteresse puntoInteresse) {
        for (Observer listener : osservatori) {
            listener.riceviNotifica(eventType, puntoInteresse);
        }
    }
    public void notifica(Boolean eventType, Materiale materiale) {

        for (Observer listener : osservatori) {
            listener.riceviNotifica(eventType, materiale);
        }
    }

    public Curatore(Comune comune,Contributor contributor) {
        super(comune,contributor);
    }

    public void share(Contenuto contenuto){
        //TODO
    }
    public void approva(Materiale materiale, boolean approvato){
        notifica(approvato, materiale);
        materiale.setApproved(approvato);
        if(approvato){
            materiale.getOwner().addMateriale(materiale);
        }else{
            materiale.getOwner().getMaterialeList().remove(materiale);
        }
    }

    public void approva(PuntoInteresse puntoInteresse, boolean approvato) {
        puntoInteresse.setApproved(true);

        notifica(approvato, puntoInteresse);
        puntoInteresse.setApproved(approvato);
        if(!approvato){
            getComune().getContenutoController().getContenuti().remove(puntoInteresse);
        }
    }


    public void delete(Contenuto contenuto){
        getComune().getContenutoController().deleteContenuto(contenuto);
    }

    @Override
    public boolean logOut(){
        //TODO
        return true;
    }
}
