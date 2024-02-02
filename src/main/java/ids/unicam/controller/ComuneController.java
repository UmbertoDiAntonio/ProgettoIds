package ids.unicam.controller;

import ids.unicam.Comune;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;


public class  ComuneController {

    private static ComuneController instance;

    public ArrayList<Comune> listaComuni = new ArrayList<>();


    public ComuneController(){}




    public static ComuneController getInstance() {
        if(instance == null){
            instance = new ComuneController();
        }
        return instance;
    }

    public @Nullable Comune getComune(String nome){
        return listaComuni.stream().filter(comune -> comune.getNome().equalsIgnoreCase(nome)).findFirst().orElse(null);
    }



}
