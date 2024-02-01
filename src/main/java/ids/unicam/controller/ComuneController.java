package ids.unicam.controller;

import ids.unicam.Comune;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.Id;
import jakarta.persistence.OneToMany;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;

@Entity
public class  ComuneController {

    private static ComuneController instance;
    @OneToMany
    public ArrayList<Comune> listaComuni = new ArrayList<>();
    @Id
    @GeneratedValue
    private Long id;

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


    public void setId(Long id) {
        this.id = id;
    }

    public Long getId() {
        return id;
    }
}
