package ids.unicam.models.attori;

import ids.unicam.Comune;
import ids.unicam.controller.ComuneController;
import ids.unicam.models.contenuti.PuntoInteresse;
import jakarta.persistence.*;

import java.util.ArrayList;


public class Turista {

    public ArrayList<PuntoInteresse> search(String searchedTags){
        ComuneController comuneController = ComuneController.getInstance();
        ArrayList<Comune> comuni = comuneController.listaComuni;
        ArrayList<PuntoInteresse> result=new ArrayList<>();
        for(Comune comune:comuni){
            for(PuntoInteresse puntoInteresse:comune.getContenutoController().getContenuti()){
                if(puntoInteresse.isApproved() && puntoInteresse.getTags().contains(searchedTags)){
                    result.add(puntoInteresse);
                }
            }
        }
        return result;
    }
    public void report(PuntoInteresse puntoInteresse){
        throw new UnsupportedOperationException();
        //TODO segnala un contenuto al Curatore
    }

    /**
     * Ti fa accedere con a uno dei tuoi account, che varia in base al comune selezionato
     */
    public void accedi(){
        throw new UnsupportedOperationException();
        //TODO
    }
}
