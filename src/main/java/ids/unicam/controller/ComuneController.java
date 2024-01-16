package ids.unicam.controller;

import ids.unicam.models.Comune;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;

public class ComuneController {
    private static  ComuneController instance;

    public ComuneController getInstance(){
        if(instance==null)
            instance=new ComuneController();
        return instance;
    }
    private ComuneController(){}
    private final ArrayList<Comune> comuni =new ArrayList<>();

    public ArrayList<Comune> getComuni() {
        return comuni;
    }

    public @Nullable Comune getComuneByName(String nomeComune){
        return comuni.stream().filter(comune -> comune.getNome().equalsIgnoreCase(nomeComune)).findFirst().orElse(null);
    }
}
